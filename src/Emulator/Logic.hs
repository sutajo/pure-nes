{-# LANGUAGE OverloadedStrings #-}

module Emulator.Logic (
  advanceEmulation,
  loadNes,
  sdlWindowWidth,
  sdlWindowHeight,
  isSave,
  ViewFunction
) where


import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Control.Concurrent
import           Control.Exception
import           Data.Maybe
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word
import           Data.IORef
import           Data.Time
import qualified Data.Text.IO                 as TIO
import           Graphics.Rendering.OpenGL as OpenGL hiding (Load, scale)
import           SDL hiding (Error)
import           SDL.Raw.Haptic
import           System.FilePath
import           System.Directory
import           Communication as Comms
import           Emulator.JoyControls as JoyControls
import           Emulator.AppResources
import qualified Nes.Controls          as Controls (Input(..), Button(..))
import           Nes.Cartridge.INES.Parser hiding (serialize, deserialize)
import           Nes.Emulation.Monad
import           Nes.CPU.Disassembler
import           Nes.CPU.Memory
import           Nes.CPU.Emulation     as CPU
import qualified Nes.CPU.Serialization as CPUS
import qualified Nes.PPU.Emulation     as PPU
import           Nes.Emulation.MasterClock
import           Nes.Serialization (serialize, serializeToFile, deserializeFromFile)


-- | Constants

nesScreenWidth  = 256
nesScreenHeight = 240

scale = 4

sdlWindowWidth  = nesScreenWidth  * scale
sdlWindowHeight = nesScreenHeight * scale


-- | Used to create timestamps for save and load actions.
formatResultTime :: IO String
formatResultTime = do
  tz <- getCurrentTimeZone
  formatTime defaultTimeLocale "%H:%M:%S" . utcToLocalTime tz <$> getCurrentTime


isSave path = ".purenes" `isExtensionOf` path


loadNes path = do
  if isSave path then do
    deserializeFromFile path
  else do
    cartridge <- loadCartridge path
    powerUpNes cartridge


-- | Send an event to the GUI through a Chan
sendEvent AppResources{..} = writeChan (fromEmulatorWindow commRes)


-- | Write the NES state to a file
save haptic nes appRes path = do
  let
    tryToSave t = do
      serializeToFile path nes
      sendEvent appRes (SaveResult $ IOResult Nothing t)
      rumble haptic

    onError (e :: SomeException) t = do
      print e
      sendEvent appRes (SaveResult $ IOResult (Just $ show e) t)
  liftIO $ do
    t <- formatResultTime
    tryToSave t `catch` (`onError` t)


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
      print e
      sendEvent appRes (LoadResult $ IOResult (Just $ show e) t)

  t <- formatResultTime
  tryToLoad t `catch` (`onError` t)


-- | Rumble the joystick if it has haptic feedback
rumble (Just haptic) = void $ hapticRumblePlay haptic 3.0 100
rumble Nothing = pure ()


-- | Convert the SDL event to the internal command type
translateSDLEvent :: SDL.EventPayload -> Maybe Command
translateSDLEvent SDL.QuitEvent = Just (Quit True)
translateSDLEvent (WindowResizedEvent (WindowResizedEventData _ (V2 w h))) = Just $ AdjustViewport w h
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
      KeycodeE     -> onlyOnPress StepClockCycle
      KeycodeR     -> onlyOnPress SwitchWindowMode
      KeycodeT     -> onlyOnPress ToggleCrtShader
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


type ViewFunction = AppResources -> VSM.IOVector Word8 -> Emulator Nes ()

-- | Execute a single command
executeCommand ::
    ViewFunction ->
    AppResources ->
    Command ->
    Emulator Nes ()
executeCommand updateScreen appResources@AppResources{..} command = do
  joys            <- liftIO $ readIORef joys
  maybeSaveFolder <- liftIO $ readIORef saveFolder

  let
    sendEvent = writeChan (fromEmulatorWindow commRes)
    checkFolderPresence path action = do
      folderExists <- doesDirectoryExist path
      if folderExists
      then action
      else sendEvent $ MessageText "The selected save folder has been removed." Alert

    withQuickSave f =
      maybe
      (sendEvent $ MessageText "Please select a save folder." Alert)
      (\folder -> checkFolderPresence folder $ f appResources (folder </> "quick.purenes"))
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
          sendEvent (MessageText (show QuickSaveNotFound) Alert)

    Save path -> do
      nes <- serialize
      liftIO $ save Nothing nes appResources path

    Load path -> do
      liftIO $ load Nothing appResources path

    JoyButtonCommand eventData -> do
      controllerId <- liftIO $ readIORef joyIsSecondCtrl
      liftIO (manageButtonEvent (toEnum.fromEnum $ controllerId) joys eventData) >>= \case
        Just command -> executeCommand updateScreen appResources command
        _            -> pure ()

    JoyHatCommand eventData -> do
      controllerId <- liftIO $ readIORef joyIsSecondCtrl
      emulateCPU $ liftIO (manageHatEvent    joys eventData) >>= mapM_ (`processInput` fromEnum controllerId)

    JoyDeviceCommand eventData -> do
      liftIO (manageDeviceEvent joys eventData)

    PlayerOneInput input -> do
      emulateCPU $ input `processInput` 0

    PlayerTwoInput input -> do
      emulateCPU $ input `processInput` 1

    SwitchEmulationMode shouldForward -> do
      screen <- emulatePPU PPU.accessScreen
      liftIO $ do
        modifyIORef' continousMode not
        when shouldForward (sendEvent (TogglePause False))
        continous <- readIORef continousMode
        unless continous (liftIO $ VSM.set screen 0)
        putStrLn ("Switched to " ++ (if continous then "continous" else "step-by-step") ++ " mode.")

    StepClockCycle -> do
      whenM (liftIO $ readIORef continousMode <&> not) $ do
        oldFrameCount <- emulatePPU PPU.getFrameCount
        emulateCPU $ do
          syncCPUwithPPU
          pc    <- readReg pc
          bytes <- mapM CPU.read [pc..pc+20]
          liftIO $ do
            TIO.putStrLn "======| Next instructions:"
            TIO.putStrLn $ disassemble pc bytes
        (pixels, newFrameCount)  <- emulatePPU $ do
          PPU.drawPalette
          (,) <$> PPU.accessScreen <*> PPU.getFrameCount
        when (newFrameCount /= oldFrameCount) (liftIO $ VSM.set pixels 0)
        updateScreen appResources pixels


    StepOneFrame -> do
      whenM (liftIO $ readIORef continousMode <&> not) $ do
        emulateFrame >>= updateScreen appResources
        emulateCPU CPUS.serializeRegisters >>= liftIO . print

    ToggleJoyMap -> liftIO $ do
      modifyIORef' joyIsSecondCtrl not
      id <- readIORef joyIsSecondCtrl
      putStr "Joy has been remapped to player "
      print (fromEnum id + 1)

    SwitchWindowMode -> do
      liftIO $ whenM (readIORef continousMode) $ do
        inFullScreen <- readIORef fullscreen

        if inFullScreen
        then do
          cursorVisible $= True
          setWindowMode window Windowed
          windowSize window $= V2 (fromIntegral sdlWindowWidth) (fromIntegral sdlWindowHeight)
          viewport $= (Position 0 0, Size (fromIntegral sdlWindowWidth) (fromIntegral sdlWindowHeight))
        else do
          cursorVisible $= False
          setWindowMode window FullscreenDesktop

        modifyIORef' fullscreen not

    AdjustViewport w h -> do
      viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

    ToggleCrtShader -> liftIO $ do
      modifyIORef' useCrtShader not

    Quit notifyGUI -> liftIO $ do
      when notifyGUI $ writeChan (fromEmulatorWindow commRes) SDLWindowClosed
      writeIORef stop True


-- | Poll events from both the SDL Window and the GUI
pollCommands :: AppResources -> IO [Command]
pollCommands res@AppResources{..} = do
  sdlCommands <- mapMaybe (translateSDLEvent . eventPayload) <$> SDL.pollEvents
  gtkCommands <- atomically $ readAllTChan (toEmulatorWindow commRes)
  return (gtkCommands ++ sdlCommands)


advanceEmulation ::
    ViewFunction ->
    AppResources ->
    Emulator Nes Bool
advanceEmulation updateScreen appResources@AppResources{continousMode, reboot, stop} = do
  commands <- liftIO $ pollCommands appResources
  mapM_ (executeCommand updateScreen appResources) commands
  whenM (liftIO $ readIORef continousMode) $ do
    emulateFrame >>= updateScreen appResources

  liftIO $ do
    reboot <- readIORef reboot
    quit   <- readIORef stop
    return (quit || reboot)
