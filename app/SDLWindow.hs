{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, GADTs #-}

module SDLWindow (
  runEmulatorWindow
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Loops
import           Control.Exception
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word
import           Data.Int
import           Data.IORef
import           Data.StateVar
import           Data.Time
import qualified Data.Text as T
import           System.Exit
import           SDL
import           SDL.Audio
import qualified SDL.Image as Img
import           SDL.Input.Joystick
import           Foreign
import           Communication
import           Nes.Cartridge
import           Nes.EmulatorMonad
import qualified Nes.CPUEmulator as CPU
import qualified Nes.PPUEmulator as PPU
import           Nes.Timing
import           Nes.Controls

scale :: Int
scale = 4

width :: Int
width = 256

height :: Int
height = 240

data Command 
  = Quit 
  | GameInput Input 
  | SwitchEmulationMode -- switches between continous and step-by-step emulation
  | StepOneFrame
  | StepClockCycle
  deriving (Eq)

translateSDLEvent :: SDL.EventPayload -> [Command]
translateSDLEvent SDL.QuitEvent = [Quit]
translateSDLEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ motion _ sym)) =
  let 
    action = case motion of
      Pressed  -> id
      Released -> const []
    inputs = case keysymKeycode sym of
      KeycodeSpace -> [SwitchEmulationMode]
      KeycodeF     -> [StepOneFrame]
      KeycodeC     -> [StepClockCycle]
      _            -> []
  in action $ inputs
translateSDLEvent _ = []

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
  window     :: Window,
  renderer   :: Renderer,
  screen     :: Texture,
  nes        :: Nes,
  commRes    :: CommResources,
  continousMode :: IORef Bool
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
  return AppResources{..}

releaseResources AppResources{..} = do
  writeChan (fromSDLWindow commRes) SDLWindowClosed
  destroyTexture screen
  destroyRenderer renderer
  destroyWindow window
  SDL.quit

runApp appResources = runEmulator (nes appResources) $ do
  CPU.reset
  updateWindow appResources `cappedAt` 60

pollCommands res@AppResources{..} = do
  sdlCommands <- (concatMap (translateSDLEvent . eventPayload)) <$> SDL.pollEvents
  gtkCommands <- (concatMap translateParentMessage) <$> (atomically $ readAllTChan (toSDLWindow commRes))
  return (gtkCommands ++ sdlCommands)

updateScreen :: AppResources -> VSM.IOVector Word8 -> IO ()
updateScreen AppResources{..} pixels = do
  (texPtr, _) <- SDL.lockTexture screen Nothing
  VSM.unsafeWith pixels $ \ptr ->  do
      copyBytes (castPtr texPtr) ptr (VSM.length pixels)
  SDL.unlockTexture screen
  copy renderer screen Nothing Nothing
  present renderer

executeCommand :: AppResources -> Command -> Emulator ()
executeCommand AppResources{..} command = case command of
  SwitchEmulationMode -> liftIO $ modifyIORef' continousMode not
  StepClockCycle      -> 
    onlyWhen not continousMode $ do
      CPU.clock >> CPU.getSnapshot >>= liftIO . print
  _                   -> pure ()

updateWindow :: AppResources -> NominalDiffTime -> Emulator Bool
updateWindow appResources@AppResources{..} dt = do
  commands <- liftIO $ pollCommands appResources
  mapM_ (executeCommand appResources) commands
  onlyWhen id continousMode $ do
    PPU.clock
    pixels  <- PPU.accessScreen
    liftIO $ updateScreen appResources pixels 
  return (Quit `elem` commands)

    