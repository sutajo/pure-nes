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

scale :: Int
scale = 4

width :: Int
width = 256

height :: Int
height = 240

data Intent = Quit | KeyPress SDL.Keysym deriving (Eq)

processEvent :: SDL.EventPayload -> IO [Intent]
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ k)) = print (SDL.keysymKeycode k) >> return [KeyPress k]
processEvent SDL.QuitEvent = return [Quit]
processEvent _ = pure []

processParentMessage :: ParentMessage -> IO [Intent]
processParentMessage Stop = pure [Quit]
processParentMessage TraceRequest = print "TraceRequest" >> pure []

greetings :: IO ()
greetings = do
  putStrLn "Starting Pure-Nes Emulator."
  putStr "SDL Version: "
  print =<< SDL.version
  putStr "Detected audio devices:"
  print =<< SDL.getAudioDeviceNames ForPlayback

bye = putStrLn "Emulator closed successfully."

data AppResources = AppResources {
  window     :: Window,
  renderer   :: Renderer,
  screen     :: Texture,
  nes        :: Nes,
  commRes    :: CommResources
}

runEmulatorWindow :: FilePath -> CommResources -> IO ()
runEmulatorWindow romPath comms = do
  greetings
  bracket (acquireResources romPath comms) releaseResources runApp 
  bye

acquireResources romPath comms = do
  cartridge <- loadCartridge romPath `except` (comms, "Error: Failed to load cartridge.")
  nes       <- powerUpNes cartridge
  SDL.initializeAll
  let windowConfig = SDL.defaultWindow {
    windowInitialSize = V2 (fromIntegral $ width * scale) (fromIntegral $ height * scale)
  }
  window    <- SDL.createWindow "Pure-Nes Emulator" windowConfig
  let commRes = comms
  let rendererConfig = RendererConfig {
    rendererType          = AcceleratedRenderer,
    rendererTargetTexture = True
  }
  renderer <- SDL.createRenderer window (-1) rendererConfig
  screen   <- SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessStreaming (V2 256 240)
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

pollIntents AppResources{..} = do
  sdlIntents <- fmap concat $ mapM (processEvent . SDL.eventPayload) =<< SDL.pollEvents
  gtkIntents <- fmap concat $ mapM processParentMessage =<< (atomically $ readAllTChan (toSDLWindow commRes))
  return (gtkIntents ++ sdlIntents)

updateScreen :: AppResources -> VSM.IOVector Word8 -> IO ()
updateScreen AppResources{..} pixels = do
  (texPtr, _) <- SDL.lockTexture screen Nothing
  VSM.unsafeWith pixels $ \ptr ->  do
      copyBytes (castPtr texPtr) ptr (VSM.length pixels)
  SDL.unlockTexture screen
  copy renderer screen Nothing Nothing
  present renderer

updateWindow :: AppResources -> NominalDiffTime -> Emulator Bool
updateWindow appResources@AppResources{..} dt = do
  CPU.getSnapshot >>= liftIO . print
  CPU.clock
  intents <- liftIO $ pollIntents appResources
  pixels  <- PPU.accessScreen
  liftIO $ updateScreen appResources pixels 
  return (Quit `elem` intents)

    