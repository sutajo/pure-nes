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
import           SDL
import           SDL.Raw.Haptic
import           SDL.Raw.Types (Haptic)
import           System.FilePath
import           System.Directory
import           Foreign hiding (void)
import           Communication as Comms
import           JoyControls
import           Text.RawString.QQ
import           Timing
import           Nes.Cartridge.Parser hiding (serialize, deserialize)
import           Nes.Emulation.Monad
import qualified Nes.CPU.Emulation as CPU
import qualified Nes.CPU.Serialization as CPUS
import qualified Nes.PPU.Emulation as PPU
import qualified Nes.Controls as Controls (Input(..), Button(..))
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

data AppResources = AppResources {
  window          :: Window,
  renderer        :: Renderer,
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

loadErrorMsg = [r|
Could not deserialize Nes from the save file.
You may have created this save on a different
CPU architecture, or this file is not a valid 
save at all.
|]

runEmulatorWindow :: FilePath -> CommResources -> IO ()
runEmulatorWindow romPath comms = do
  bracket (acquireResources romPath comms) releaseResources runApp 
  bye

isSave path = ".purenes" `isExtensionOf` path

rumbleJoy :: Joystick -> IO ()
rumbleJoy joy = return ()

loadNes path = do
  if isSave path then do
    file <- B.readFile path
    case decode file of
      Left  err -> fail err
      Right nes -> deserialize nes 
  else do
    cartridge <- loadCartridge path
    powerUpNes cartridge

acquireResources romPath comms = do
  reboot    <- newIORef False
  nes       <- (loadNes romPath >>= newIORef) `except` (comms, "Error: Failed to load cartridge.")
  SDL.initializeAll
  greetings
  let windowConfig = SDL.defaultWindow {
    windowInitialSize = V2 (fromIntegral $ width * scale) (fromIntegral $ height * scale),
    windowResizable = True,
    windowPosition = Absolute $ P $ V2 0 20
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
  let buttonMappings = M.fromList [(0, Controls.Select), (1, Controls.Start), (2, Controls.A), (3, Controls.B)]
  joys          <- JoyControls.init buttonMappings >>= newIORef
  saveFolder    <- newIORef Nothing
  reset         <- newIORef (not $ isSave romPath)
  joyIsSecondCtrl <- newIORef False
  fullscreen      <- newIORef False
  return AppResources{..}

releaseResources AppResources{..} = do
  writeChan (fromSDLWindow commRes) SDLWindowClosed
  destroyTexture screen
  destroyRenderer renderer
  destroyWindow window
  SDL.quit

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

pollCommands res@AppResources{..} = do
  sdlCommands <- (catMaybes . map (translateSDLEvent . eventPayload)) <$> SDL.pollEvents
  gtkCommands <- atomically $ readAllTChan (toSDLWindow commRes)
  return (gtkCommands ++ sdlCommands)

updateScreen :: AppResources -> VSM.IOVector Word8 -> Emulator ()
updateScreen AppResources{..} pixels = liftIO $ do
  (texPtr, _) <- SDL.lockTexture screen Nothing
  VSM.unsafeWith pixels $ \ptr ->  do
      copyBytes (castPtr texPtr) ptr (VSM.length pixels)
  SDL.unlockTexture screen
  copy renderer screen Nothing Nothing
  present renderer

sendEvent AppResources{..} = writeChan (fromSDLWindow commRes)

save appRes path = do
  let 
    tryToSave nes t = do
      B.writeFile path (encode nes)
      sendEvent appRes (SaveResult $ IOResult Nothing t)
      return True

    onError (e :: SomeException) t = do
      print (show e)
      sendEvent appRes (SaveResult $ IOResult (Just $ show e) t)
      return False

  nes <- serialize
  liftIO $ do
    t <- formatResultTime 
    tryToSave nes t `catch` (\ e -> onError e t) 

load appRes@AppResources{..} path = liftIO $ do
  let 
    tryToLoad t = do
      newNes <- liftIO $ loadNes path
      writeIORef nes newNes
      writeIORef reboot True
      writeIORef reset (not $ isSave path)
      sendEvent appRes (LoadResult $ IOResult Nothing t)
      return True

    onError (e :: SomeException) t = do 
      print (show e)
      sendEvent appRes (LoadResult $ IOResult (Just $ show e) t)
      return False

  t <- formatResultTime
  tryToLoad t `catch` (\ e -> onError e t)

rumbleOnSuccess :: Maybe Haptic -> Emulator Bool -> Emulator ()
rumbleOnSuccess (Just haptic) cond = whenM cond $ do
  ret <- hapticRumblePlay haptic 3.0 100  
  when (ret /= 0) (liftIO $ putStrLn "Rumble failed")
rumbleOnSuccess _ cond = void cond

executeCommand :: AppResources -> Command -> Emulator ()
executeCommand appResources@AppResources{..} command = do
  joys            <- liftIO $ readIORef joys
  maybeSaveFolder <- liftIO $ readIORef saveFolder 

  let 
    sendEvent = writeChan (fromSDLWindow commRes)
    withQuickSave f = maybe (return False)  (\folder -> f appResources (folder </> "quick.purenes")) maybeSaveFolder
  case command of
    NewSaveFolder s -> liftIO $ writeIORef saveFolder s

    QuickSave haptic ->
      rumbleOnSuccess haptic $ 
        withQuickSave save

    QuickLoad haptic ->
      rumbleOnSuccess haptic $  
        withQuickSave $ \appResources path -> do
          saveExists <- liftIO $ doesFileExist path
          if saveExists
          then load appResources path 
          else do
            liftIO $ putStrLn "No quicksave file found."
            return False

    Save path -> void $ save appResources path

    Load path -> void $ load appResources path

    JoyButtonCommand eventData -> do
      controllerId <- liftIO $ readIORef joyIsSecondCtrl
      liftIO (manageButtonEvent (fromEnum controllerId) joys eventData) >>= \case
        Just command -> executeCommand appResources command
        _            -> pure ()

    JoyHatCommand eventData    -> do
      controllerId <- liftIO $ readIORef joyIsSecondCtrl
      liftIO (manageHatEvent    joys eventData) >>= mapM_ (`processInput` fromEnum controllerId)

    JoyDeviceCommand eventData -> liftIO (manageDeviceEvent joys eventData)

    PlayerOneInput input -> input `processInput` 0

    PlayerTwoInput input -> input `processInput` 1
    
    SwitchEmulationMode shouldForward -> do
        stepByStep <- liftIO $ do
          modifyIORef' continousMode not
          when shouldForward $ sendEvent (SwitchMode False)
          continous <- readIORef continousMode
          putStrLn ("Switched to " ++ (if continous then "continous" else "step-by-step") ++ " mode.")
          return $ not continous
        pixels  <- PPU.accessScreen
        liftIO $ VSM.set pixels 0

    StepClockCycle -> 
      whenM (liftIO $ readIORef continousMode <&> not) $ do
        replicateM_ 100 clocks
        PPU.drawPalette
        PPU.drawSprites
        pixels  <- PPU.accessScreen
        updateScreen appResources pixels

    StepOneFrame ->
      whenM (liftIO $ readIORef continousMode <&> not) $ do
        emulateFrame
        CPUS.serialize >>= liftIO . print
        pixels  <- PPU.accessScreen
        updateScreen appResources pixels

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

updateWindow :: AppResources -> Emulator Bool
updateWindow appResources@AppResources{..} = do
  commands <- liftIO $ pollCommands appResources
  mapM_ (executeCommand appResources) commands
  whenM (liftIO $ readIORef continousMode) $ do
    emulateFrame
    PPU.accessScreen >>= updateScreen appResources
  reboot <- liftIO $ readIORef reboot
  return (Quit `elem` commands || reboot)

    