{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, GADTs #-}

module SDLWindow (
  runEmulatorWindow
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.Map                     as M
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Maybe
import           Data.Word
import           Data.IORef
import           Data.Time
import           SDL
import           Foreign hiding (void)
import           Communication
import           JoyControls
import           Nes.Cartridge
import           Nes.EmulatorMonad
import qualified Nes.CPUEmulator as CPU
import qualified Nes.PPUEmulator as PPU
import           Nes.Timing
import           Nes.Controls as Controls (Input(..), Button(..))
import           Nes.MasterClock

scale :: Int
scale = 4

width :: Int
width = 256

height :: Int
height = 240

data Command 
  = Quit 
  | PlayerInput Input 
  | SwitchEmulationMode -- switches between continous and step-by-step emulation
  | JoyButtonCommand JoyButtonEventData
  | JoyDeviceCommand JoyDeviceEventData
  | JoyHatCommand JoyHatEventData
  | StepOneFrame
  | StepClockCycle
  deriving (Eq)

translateSDLEvent :: SDL.EventPayload -> Maybe Command
translateSDLEvent SDL.QuitEvent = Just Quit
translateSDLEvent (JoyHatEvent eventData)    = Just $ JoyHatCommand eventData
translateSDLEvent (JoyButtonEvent eventData) = Just $ JoyButtonCommand eventData
translateSDLEvent (JoyDeviceEvent eventData) = Just $ JoyDeviceCommand eventData
translateSDLEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ motion _ sym)) =
  let 
    onlyOnPress = case motion of
      Pressed  -> Just
      Released -> const Nothing
    playerInput x = Just . PlayerInput $ case motion of
      Pressed  -> Press x
      Released -> Release x
    inputs = case keysymKeycode sym of
      KeycodeSpace -> onlyOnPress SwitchEmulationMode
      KeycodeF     -> onlyOnPress StepOneFrame
      KeycodeC     -> onlyOnPress StepClockCycle
      KeycodeUp    -> playerInput Up
      KeycodeDown  -> playerInput Down
      KeycodeLeft  -> playerInput Controls.Left
      KeycodeRight -> playerInput Controls.Right
      Keycode1     -> playerInput A
      Keycode2     -> playerInput B
      Keycode3     -> playerInput Select
      Keycode4     -> playerInput Start
      _            -> Nothing
  in inputs
translateSDLEvent _ = Nothing

translateParentMessage :: ParentMessage -> [Command]
translateParentMessage Stop = [Quit]
translateParentMessage TraceRequest = []

greetings :: IO ()
greetings = do
  putStrLn "Starting Pure-Nes Emulator."
  putStr "SDL Version: "
  print =<< SDL.version
  putStr "Detected audio devices: "
  print =<< SDL.getAudioDeviceNames ForPlayback

bye = putStrLn "Emulator closed successfully."

data AppResources = AppResources {
  window          :: Window,
  renderer        :: Renderer,
  screen          :: Texture,
  nes             :: Nes,
  commRes         :: CommResources,
  continousMode   :: IORef Bool,
  joys            :: IORef JoyControlState
}

onlyWhen :: MonadIO m => (a -> Bool) -> IORef a -> m () -> m ()
onlyWhen f ref m = do
  cond <- liftIO (f <$> readIORef ref)
  when cond m

runEmulatorWindow :: FilePath -> CommResources -> IO ()
runEmulatorWindow romPath comms = do
  bracket (acquireResources romPath comms) releaseResources runApp 
  bye

acquireResources romPath comms = do
  cartridge <- loadCartridge romPath `except` (comms, "Error: Failed to load cartridge.")
  nes       <- powerUpNes cartridge
  SDL.initializeAll
  greetings
  let windowConfig = SDL.defaultWindow {
    windowInitialSize = V2 (fromIntegral $ width * scale) (fromIntegral $ height * scale)
  }
  window    <- SDL.createWindow "Pure-Nes Emulator" windowConfig
  let commRes = comms
  let rendererConfig = RendererConfig {
    rendererType          = AcceleratedRenderer,
    rendererTargetTexture = True
  }
  renderer      <- SDL.createRenderer window (-1) rendererConfig
  screen        <- SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessStreaming (V2 256 240)
  continousMode <- newIORef True
  let buttonMappings = M.fromList [(0, Select), (1, Start), (2, A), (3, B)]
  joys          <- JoyControls.init buttonMappings >>= newIORef
  return AppResources{..}

releaseResources AppResources{..} = do
  writeChan (fromSDLWindow commRes) SDLWindowClosed
  destroyTexture screen
  destroyRenderer renderer
  destroyWindow window
  SDL.quit

runApp appResources = runEmulator (nes appResources) $ do
  CPU.reset
  PPU.reset
  uncapped $ updateWindow appResources

pollCommands res@AppResources{..} = do
  sdlCommands <- (catMaybes . map (translateSDLEvent . eventPayload)) <$> SDL.pollEvents
  gtkCommands <- (concatMap translateParentMessage) <$> (atomically $ readAllTChan (toSDLWindow commRes))
  return (gtkCommands ++ sdlCommands)

updateScreen :: AppResources -> VSM.IOVector Word8 -> Emulator ()
updateScreen AppResources{..} pixels = liftIO $ do
  (texPtr, _) <- SDL.lockTexture screen Nothing
  VSM.unsafeWith pixels $ \ptr ->  do
      copyBytes (castPtr texPtr) ptr (VSM.length pixels)
  SDL.unlockTexture screen
  copy renderer screen Nothing Nothing
  present renderer


executeCommand :: AppResources -> Command -> Emulator ()
executeCommand appResources@AppResources{..} command = do
  joys <- liftIO $ readIORef joys
  case command of
    JoyButtonCommand eventData -> liftIO (manageButtonEvent joys eventData) >>= mapM_ (`processInput` 0)
    JoyHatCommand eventData    -> liftIO (manageHatEvent    joys eventData) >>= mapM_ (`processInput` 0)
    JoyDeviceCommand eventData -> liftIO (manageDeviceEvent joys eventData)
    PlayerInput input   -> processInput input 0
    SwitchEmulationMode -> do
        stepByStep <- liftIO $ do
          modifyIORef' continousMode not
          continous <- readIORef continousMode
          putStrLn ("Switched to " ++ (if continous then "continous" else "step-by-step") ++ " mode.")
          return $ not continous
        pixels  <- PPU.accessScreen
        liftIO $ VSM.set pixels 0
    StepClockCycle -> 
      onlyWhen not continousMode $ do
        clocks
        PPU.drawPalette
        pixels  <- PPU.accessScreen
        updateScreen appResources pixels
    StepOneFrame ->
      onlyWhen not continousMode $ do
        emulateFrame
        PPU.drawBackground
        pixels  <- PPU.accessScreen
        updateScreen appResources pixels
    _ -> pure ()

updateWindow :: AppResources -> NominalDiffTime -> Emulator Bool
updateWindow appResources@AppResources{..} dt = do
  commands <- liftIO $ pollCommands appResources
  mapM_ (executeCommand appResources) commands
  onlyWhen id continousMode $ do
    emulateFrame
    PPU.accessScreen >>= updateScreen appResources
  return (Quit `elem` commands)

    