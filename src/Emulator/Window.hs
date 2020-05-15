{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, GADTs, QuasiQuotes #-}

module Emulator.Window (
  runEmulatorWindow
) where

import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Exception
import           Data.Maybe
import qualified Data.Map                     as M
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.IORef
import           Graphics.Rendering.OpenGL as OpenGL hiding (Load, scale)
import           SDL hiding (Error)
import           Foreign hiding (void)
import           Communication as Comms
import           Emulator.JoyControls as JoyControls
import           Emulator.Framerate
import           Emulator.CrtShader
import           Emulator.Logic
import           Emulator.AppResources
import           Nes.Emulation.Monad
import qualified Nes.Controls as Controls
import           Nes.Emulation.MasterClock


greetings :: IO ()
greetings = do
  putStrLn "Starting Pure-Nes Emulator."
  putStr "SDL Version: "
  print =<< SDL.version
  --putStr "Detected audio devices: "
  --print =<< SDL.getAudioDeviceNames ForPlayback


bye = putStrLn "Emulator closed successfully."


-- | Main entry point of the window
runEmulatorWindow :: FilePath -> CommResources -> IO ()
runEmulatorWindow romPath comms = runInBoundThread $ do
  bracket (acquireResources romPath comms) releaseResources runApp 
  bye


acquireResources :: FilePath -> CommResources -> IO AppResources
acquireResources romPath comms = do
  reboot    <- newIORef False
  nes       <- (loadNes romPath >>= newIORef) `sendMessageOnException` comms

  setHintWithPriority OverridePriority HintRenderDriver OpenGL
  setHintWithPriority OverridePriority HintRenderOpenGLShaders EnableShaders
  SDL.initializeAll
  greetings

  let 
    windowConfig = SDL.defaultWindow {
      windowInitialSize = V2 (fromIntegral $ sdlWindowWidth) (fromIntegral $ sdlWindowHeight),
      windowResizable = True,
      windowPosition = Absolute $ P $ V2 0 20,
      windowGraphicsContext = OpenGLContext defaultOpenGL { 
        glColorPrecision = V4 8 8 8 8,
        glProfile = Core Normal 3 0
      }
  }

  window    <- SDL.createWindow "Pure-Nes Emulator" windowConfig

  let 
    buttonMappings = M.fromList [(0, Controls.Select), (1, Controls.Start), (2, Controls.A), (3, Controls.B)]
    commRes = comms
    rendererConfig = RendererConfig {
      rendererType          = AcceleratedRenderer,
      rendererTargetTexture = True
    }
  glContext     <- glCreateContext window
  renderer      <- SDL.createRenderer window (-1) rendererConfig
  swapInterval $= SynchronizedUpdates
  screen        <- SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessStreaming (V2 256 240)
  continousMode <- newIORef True
  joys          <- JoyControls.init buttonMappings >>= newIORef
  saveFolder    <- newIORef Nothing
  reset         <- newIORef (not $ isSave romPath)
  joyIsSecondCtrl <- newIORef False
  fullscreen      <- newIORef False
  stop            <- newIORef False
  useCrtShader    <- newIORef True
  openGLResources <- do
      tryResult <- try getCrtShaderProgram
      case tryResult of
        Left (ex :: SomeException) -> return Nothing
        Right crtProgram  -> do
          textureUniform  <- uniformLocation crtProgram "texImg"
          return $ Just OpenGLResources{..}

  return AppResources{..}


releaseResources :: AppResources -> IO ()
releaseResources AppResources{..} = do
  whenJust openGLResources $ \OpenGLResources{glContext} -> do
    glDeleteContext glContext

  readIORef joys >>= disconnectAllJoys
  destroyTexture screen
  destroyRenderer renderer
  destroyWindow window
  SDL.quit


-- | Main loop
runApp :: AppResources -> IO ()
runApp appResources@AppResources{..} = do
  nes           <- readIORef nes
  writeIORef reboot False
  
  shouldReset  <- readIORef reset
  runEmulator nes $ do
    when shouldReset $ do
      resetNes
    updateWindow appResources `cappedAt` 60

  shouldReboot <- readIORef reboot
  when shouldReboot $ do
    runApp appResources


-- | Update the pixels on the screen
updateScreen :: ViewFunction
updateScreen AppResources{..} pixels = liftIO $ do
  -- Upload the changes to the texture
  (texPtr, _) <- SDL.lockTexture screen Nothing
  VSM.unsafeWith pixels $ \ptr ->  do
      copyBytes (castPtr texPtr) ptr (VSM.length pixels)
  SDL.unlockTexture screen

  -- Render the texture to the screen
  useShader <- readIORef useCrtShader
  if useShader && isJust openGLResources
  then do
    let OpenGLResources{..} = fromJust openGLResources
    OpenGL.clear [ColorBuffer, DepthBuffer]
    oldProgram <- SDL.get currentProgram
    currentProgram $= Just crtProgram
    glBindTexture screen
    uniform textureUniform $= (0 :: GLint)
    let 
      mintex = 0 :: Float
      maxtex = 1 :: Float
      screenMin = -1 :: GLfloat
      screenMax = 1 :: GLfloat
    renderPrimitive TriangleStrip $ do
      texCoord (TexCoord2 mintex maxtex)
      vertex (Vertex2 screenMin screenMin)
      texCoord (TexCoord2 maxtex maxtex)
      vertex (Vertex2 screenMax screenMin)
      texCoord (TexCoord2 mintex mintex)
      vertex (Vertex2 screenMin screenMax)
      texCoord (TexCoord2 maxtex mintex)
      vertex (Vertex2 screenMax screenMax)
    glUnbindTexture screen
    currentProgram $= oldProgram
    glSwapWindow window
  else do
    SDL.clear renderer
    copy renderer screen Nothing Nothing
    present renderer

-- | Main loop body
updateWindow :: AppResources -> Emulator Nes Bool
updateWindow appResources@AppResources{..} = do
  advanceEmulation updateScreen appResources

    