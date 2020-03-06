{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, GADTs #-}

module SDLWindow (
  runEmulatorWindow
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.ByteString              as B
import qualified Data.Map                     as M
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Store
import           Data.Maybe
import           Data.Word
import           Data.IORef
import           Data.Time
import           SDL
import           System.FilePath.Posix
import           Foreign hiding (void)
import           Communication as Comms
import           JoyControls
import           Nes.Cartridge.Parser
import           Nes.Emulation.Monad
import qualified Nes.CPU.Emulation as CPU
import qualified Nes.PPU.Emulation as PPU
import           Nes.Timing
import           Nes.Controls as Controls (Input(..), Button(..))
import           Nes.Emulation.MasterClock
import           Nes.Serialization (serialize, deserialize)

formatResultTime :: IO String
formatResultTime = do
  tz <- getCurrentTimeZone
  t  <- getCurrentTime
  return $ formatTime defaultTimeLocale "%H:%M:%S" (utcToLocalTime tz t)

scale :: Int
scale = 4

width :: Int
width = 256

height :: Int
height = 240

data Command 
  = Quit
  | Save FilePath
  | Load FilePath
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
translateParentMessage (SaveVM p) = [Save p]
translateParentMessage (LoadVM p) = [Load p]
translateParentMessage Comms.Switch = [SwitchEmulationMode]
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
  commRes         :: CommResources,
  nes             :: IORef Nes,
  reboot          :: IORef Bool,
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
  reboot        <- newIORef False
  nes       <- do
    if ".purenes" `isExtensionOf` romPath then do
      writeIORef reboot True
      B.readFile romPath <&> decodeEx >>= deserialize >>= newIORef
    else do
      cartridge <- loadCartridge romPath `except` (comms, "Error: Failed to load cartridge.")
      powerUpNes cartridge >>= newIORef
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

runApp appResources@AppResources{..} = do
  nes <- readIORef nes
  rebootRequest <- readIORef reboot
  writeIORef reboot False
  runEmulator nes $ do
    when (not rebootRequest) $ do CPU.reset; PPU.reset
    updateWindow appResources `cappedAt` 60

  shouldReboot <- readIORef reboot
  when shouldReboot $ do
    runApp appResources

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
  let sendEvent = writeChan (fromSDLWindow commRes)
  joys <- liftIO $ readIORef joys
  case command of
    Save path -> do
      pureNes <- serialize
      liftIO $ do
        t <- formatResultTime
        void . forkIO $ (do
          B.writeFile path (encode pureNes)
          sendEvent (SaveError (Nothing, t))) `catch` (\(e :: SomeException) -> do print (show e); sendEvent (SaveError (Just $ show e, t)))
    Load path -> 
      liftIO $ do
        t <- formatResultTime
        (do
          newNes <- liftIO $ B.readFile path <&> decodeEx >>= deserialize
          writeIORef nes newNes
          writeIORef reboot True
          sendEvent (LoadError (Nothing, t))) `catch` (\(e :: SomeException) -> do print (show e); sendEvent (LoadError (Just $ show e, t)))
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
        replicateM_ 100 clocks
        PPU.drawPalette
        PPU.drawSprites
        pixels  <- PPU.accessScreen
        updateScreen appResources pixels
    StepOneFrame ->
      onlyWhen not continousMode $ do
        emulateFrame
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
  reboot <- liftIO $ readIORef reboot
  return (Quit `elem` commands || reboot)

    