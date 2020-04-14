{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, GADTs, QuasiQuotes #-}

module Emulator.Window (
  runEmulatorWindow
) where

import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.ByteString              as B
import qualified Data.Map                     as M
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Serialize
import           Data.Maybe
import           Data.Word
import           Data.IORef
import           Data.Time
import           SDL hiding (Error)
import           SDL.Raw.Haptic
import           System.FilePath
import           System.Directory
import           Foreign hiding (void)
import           Communication as Comms
import           Text.RawString.QQ
import           Emulator.JoyControls as JoyControls
import           Emulator.Framerate
import           Emulator.CrtShader
import           Nes.Cartridge.INES.Parser hiding (serialize, deserialize)
import           Nes.Emulation.Monad
import qualified Nes.CPU.Emulation as CPU
import qualified Nes.CPU.Serialization as CPUS
import qualified Nes.PPU.Emulation as PPU
import qualified Nes.Controls as Controls (Input(..), Button(..))
import           Nes.Emulation.MasterClock
import           Nes.Serialization (serialize, deserialize)


-- | Used to create timestamps for save and load actions.
formatResultTime :: IO String
formatResultTime = do
  tz <- getCurrentTimeZone
  t  <- getCurrentTime
  return $ formatTime defaultTimeLocale "%H:%M:%S" (utcToLocalTime tz t)



-- | Convert the SDL event to the internal command type
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
    playerInput x = Just . PlayerOneInput $ case motion of
      Pressed  -> Controls.Press x
      Released -> Controls.Release x
    inputs = case keysymKeycode sym of
      KeycodeG     -> onlyOnPress ToggleJoyMap
      KeycodeF5    -> onlyOnPress $ QuickSave Nothing
      KeycodeF9    -> onlyOnPress $ QuickLoad Nothing
      KeycodeSpace -> onlyOnPress (SwitchEmulationMode True)
      KeycodeF     -> onlyOnPress StepOneFrame
      KeycodeC     -> onlyOnPress StepClockCycle
      KeycodeR     -> onlyOnPress SwitchWindowMode
      KeycodeUp    -> playerInput Controls.Up
      KeycodeDown  -> playerInput Controls.Down
      KeycodeLeft  -> playerInput Controls.Left
      KeycodeRight -> playerInput Controls.Right
      Keycode1     -> playerInput Controls.A
      Keycode2     -> playerInput Controls.B
      Keycode3     -> playerInput Controls.Select
      Keycode4     -> playerInput Controls.Start
      _            -> Nothing
  in inputs
translateSDLEvent _ = Nothing



greetings :: IO ()
greetings = do
  putStrLn "Starting Pure-Nes Emulator."
  putStr "SDL Version: "
  print =<< SDL.version
  putStr "Detected audio devices: "
  print =<< SDL.getAudioDeviceNames ForPlayback

bye = putStrLn "Emulator closed successfully."



-- | Window resources
data AppResources = AppResources {
  window          :: Window,
  renderer        :: Renderer,
  glContext       :: GLContext,
  screen          :: Texture,
  commRes         :: CommResources,
  nes             :: IORef Nes,
  reset           :: IORef Bool,
  reboot          :: IORef Bool,
  continousMode   :: IORef Bool,
  joyIsSecondCtrl :: IORef Bool,
  joys            :: IORef JoyControlState,
  saveFolder      :: IORef (Maybe FilePath),
  fullscreen      :: IORef Bool
}



deserializeErrorMsg = [r|
Could not deserialize Nes from the save file.
This is not a save file or it may have been corrupted.
|]



-- | Main entry point of the window
runEmulatorWindow :: FilePath -> CommResources -> IO ()
runEmulatorWindow romPath comms = do
  bracket (acquireResources romPath comms) releaseResources runApp 
  bye



isSave path = ".purenes" `isExtensionOf` path

loadNes path = do
  if isSave path then do
    file <- B.readFile path
    case decode file of
      Left  err -> failure deserializeErrorMsg
      Right nes -> deserialize nes 
  else do
    cartridge <- loadCartridge path
    powerUpNes cartridge



acquireResources romPath comms = do
  reboot    <- newIORef False
  nes       <- (loadNes romPath >>= newIORef) `sendMessageOnException` comms

  SDL.initializeAll
  greetings

  let 
    width  = 256
    height = 240
    scale  = 3
    windowConfig = SDL.defaultWindow {
      windowInitialSize = V2 (fromIntegral $ width * scale) (fromIntegral $ height * scale),
      windowResizable = True,
      windowPosition = Absolute $ P $ V2 0 20,
      windowGraphicsContext = OpenGLContext defaultOpenGL
  }

  window    <- SDL.createWindow "Pure-Nes Emulator" windowConfig

  let 
    buttonMappings = M.fromList [(0, Controls.Select), (1, Controls.Start), (2, Controls.A), (3, Controls.B)]
    commRes = comms
    rendererConfig = RendererConfig {
      rendererType          = AcceleratedRenderer,
      rendererTargetTexture = True
    }

  renderer      <- SDL.createRenderer window (-1) rendererConfig
  screen        <- SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessStreaming (V2 256 240)
  continousMode <- newIORef True
  joys          <- JoyControls.init buttonMappings >>= newIORef
  saveFolder    <- newIORef Nothing
  reset         <- newIORef (not $ isSave romPath)
  joyIsSecondCtrl <- newIORef False
  fullscreen      <- newIORef False
  glContext       <- glCreateContext window
  activateCrtShader
  return AppResources{..}



releaseResources AppResources{..} = do
  readIORef joys >>= disconnectAllJoys
  glDeleteContext glContext
  destroyTexture screen
  destroyRenderer renderer
  destroyWindow window
  SDL.quit
  writeChan (fromSDLWindow commRes) SDLWindowClosed


-- | Main loop
runApp appResources@AppResources{..} = do
  nes           <- readIORef nes
  writeIORef reboot False
  
  shouldReset  <- readIORef reset
  runEmulator nes $ do
    when shouldReset $ CPU.reset; PPU.reset
    updateWindow appResources `cappedAt` 60

  shouldReboot <- readIORef reboot
  when shouldReboot $ do
    runApp appResources


-- | Poll events from both the SDL Window and the GUI
pollCommands res@AppResources{..} = do
  sdlCommands <- (catMaybes . map (translateSDLEvent . eventPayload)) <$> SDL.pollEvents
  gtkCommands <- atomically $ readAllTChan (toSDLWindow commRes)
  return (gtkCommands ++ sdlCommands)


-- | Update the pixels on the screen
updateScreen :: AppResources -> VSM.IOVector Word8 -> Emulator ()
updateScreen AppResources{..} pixels = liftIO $ do
  (texPtr, _) <- SDL.lockTexture screen Nothing
  VSM.unsafeWith pixels $ \ptr ->  do
      copyBytes (castPtr texPtr) ptr (VSM.length pixels)
  SDL.unlockTexture screen
  copy renderer screen Nothing Nothing
  present renderer


-- | Send an event to the GUI through a Chan
sendEvent AppResources{..} = writeChan (fromSDLWindow commRes)


-- | Write the NES state to a file
save haptic nes appRes path = do
  let 
    tryToSave t = do
      B.writeFile path (encode nes)
      sendEvent appRes (SaveResult $ IOResult Nothing t)
      rumble haptic

    onError (e :: SomeException) t = do
      print (show e)
      sendEvent appRes (SaveResult $ IOResult (Just $ show e) t)
  liftIO $ do
    t <- formatResultTime 
    tryToSave t `catch` (\ e -> onError e t) 


-- | Read the NES state from a file
load haptic appRes@AppResources{..} path = do
  let 
    tryToLoad t = do
      newNes <- liftIO $ loadNes path
      writeIORef nes newNes
      writeIORef reboot True
      writeIORef reset (not $ isSave path)
      sendEvent appRes (LoadResult $ IOResult Nothing t)
      rumble haptic   

    onError (e :: SomeException) t = do 
      print (show e)
      sendEvent appRes (LoadResult $ IOResult (Just $ show e) t)

  t <- formatResultTime
  tryToLoad t `catch` (\ e -> onError e t)


-- | Rumble the joystick if it has haptic feedback
rumble (Just haptic) = void $ hapticRumblePlay haptic 3.0 100
rumble Nothing = pure ()


-- | Execute a single command
executeCommand :: AppResources -> Command -> Emulator ()
executeCommand appResources@AppResources{..} command = do
  joys            <- liftIO $ readIORef joys
  maybeSaveFolder <- liftIO $ readIORef saveFolder 

  let 
    sendEvent = writeChan (fromSDLWindow commRes)

    withQuickSave f = 
      maybe 
      (sendEvent $ MessageText "Please select a save folder." Alert)  
      (\folder -> f appResources (folder </> "quick.purenes")) 
      maybeSaveFolder

  case command of

    NewSaveFolder s -> liftIO $ do
      writeIORef saveFolder s

    QuickSave haptic -> do
      nes <- serialize
      liftIO . void . forkIO $ withQuickSave (save haptic nes)

    QuickLoad haptic -> liftIO $
      withQuickSave $ \appResources path -> do
        saveExists <- doesFileExist path
        if saveExists
        then load haptic appResources path 
        else do
          sendEvent =<< (LoadResult . IOResult (Just . show $ QuickSaveNotFound) <$> formatResultTime)

    Save path -> do
      nes <- serialize
      liftIO $ save Nothing nes appResources path

    Load path -> do
      liftIO $ load Nothing appResources path

    JoyButtonCommand eventData -> do
      controllerId <- liftIO $ readIORef joyIsSecondCtrl
      liftIO (manageButtonEvent (fromEnum controllerId) joys eventData) >>= \case
        Just command -> executeCommand appResources command
        _            -> pure ()

    JoyHatCommand eventData -> do
      controllerId <- liftIO $ readIORef joyIsSecondCtrl
      liftIO (manageHatEvent    joys eventData) >>= mapM_ (`processInput` fromEnum controllerId)

    JoyDeviceCommand eventData -> do
      liftIO (manageDeviceEvent joys eventData)

    PlayerOneInput input -> do
      input `processInput` 0

    PlayerTwoInput input -> do
      input `processInput` 1
    
    SwitchEmulationMode shouldForward -> do
      stepByStep <- liftIO $ do
        modifyIORef' continousMode not
        when shouldForward $ sendEvent (SwitchMode False)
        continous <- readIORef continousMode
        putStrLn ("Switched to " ++ (if continous then "continous" else "step-by-step") ++ " mode.")
        return $ not continous
      pixels  <- PPU.accessScreen
      liftIO $ VSM.set pixels 0

    StepClockCycle -> do
      whenM (liftIO $ readIORef continousMode <&> not) $ do
        replicateM_ 100 execCpuInstruction
        PPU.drawPalette
        PPU.drawSprites
        pixels  <- PPU.accessScreen
        updateScreen appResources pixels

    StepOneFrame -> do
      whenM (liftIO $ readIORef continousMode <&> not) $ do
        emulateFrame >>= updateScreen appResources
        CPUS.serialize >>= liftIO . print

    ToggleJoyMap -> liftIO $ do
      modifyIORef' joyIsSecondCtrl not
      id <- readIORef joyIsSecondCtrl
      putStr "Joy has been remapped as controller "
      print (fromEnum id)

    SwitchWindowMode -> liftIO $ do
      inFullScreen <- readIORef fullscreen
      setWindowMode window (if inFullScreen then Windowed else FullscreenDesktop)
      modifyIORef' fullscreen not
      
    _ -> pure ()


-- | Main loop body
updateWindow :: AppResources -> Emulator Bool
updateWindow appResources@AppResources{..} = do
  commands <- liftIO $ pollCommands appResources
  mapM_ (executeCommand appResources) commands

  whenM (liftIO $ readIORef continousMode) $ do
    emulateFrame >>= updateScreen appResources

  reboot <- liftIO $ readIORef reboot
  return (Quit `elem` commands || reboot)

    