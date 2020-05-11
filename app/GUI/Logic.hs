{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Concurrent       hiding (yield)
import           Control.Concurrent.STM
import qualified Data.Text                     as Text
import           GI.Gtk.Declarative.App.Simple as DAS
import           System.FilePath
import           System.Directory
import           Pipes
import           Emulator.Window
import           GUI.Window
import           GUI.State
import           Communication


-- | Launch the emulator on a new, bound thread
launchEmulator :: FilePath -> CommResources -> IO ()
launchEmulator path comms = do
  toEmulatorWindow' <- atomically $ dupTChan (toEmulatorWindow comms)
  void . forkOS $ runEmulatorWindow path comms { toEmulatorWindow = toEmulatorWindow' }


noop :: Monad m => m (Maybe a)
noop = pure Nothing

only :: Monad m => m a -> m (Maybe b)
only x = do x; noop

emit :: Monad m => a -> m (Maybe a)
emit = return . Just


resultEvent :: Monad m => IOResult -> String -> m (Maybe Event)
resultEvent result op = 
  let prefix = "Oops! Something went wrong during " ++ op ++ ":\n" in
  case errorMsg result of
    Nothing  -> noop
    Just msg -> emit $ MessageText (prefix ++ msg) Cross



chooseSaveFolderFirst :: State -> State
chooseSaveFolderFirst = Message "You need to choose a save folder first." Alert


sendMsg :: CommResources -> Command -> IO (Maybe Event)
sendMsg CommResources{toEmulatorWindow} = only . atomically . writeTChan toEmulatorWindow

-- | If a field should change while displaying a message
--   then update the nested emulation state
updateNestedEmulationState :: (State -> State) -> State -> State
updateNestedEmulationState f e@Message{stateBefore = s} = e{stateBefore = updateNestedEmulationState f s}
updateNestedEmulationState f e@Emulating{} = f e
updateNestedEmulationState f x = x


endAnimationWithDelay CommResources{fromEmulatorWindow} = forkIO $ do
  threadDelay (10^2 * 2)
  writeChan fromEmulatorWindow $ EndAnimation

smoothTransitionAction :: CommResources -> State -> IO a -> Transition State Event
smoothTransitionAction comms s action = Transition (SmoothTransition s) (action >> only (endAnimationWithDelay comms)) 

smoothTransition :: CommResources -> State -> Transition State Event
smoothTransition comms s = smoothTransitionAction comms s (pure ())

-- | GUI state transition function
update :: CommResources -> State -> Event -> Transition State Event

-- New filepath selected in the main menu

update _ s@MainMenu{} (FileSelectionChanged p) 
  = Transition s{selectedRom = p} (return Nothing)


-- Show controls and then return if Ok was pressed

update comms s@MainMenu{} ShowControlsPressed = smoothTransition comms (ShowControls s)

update comms (ShowControls s) MessageAck = smoothTransition comms s


-- Start the emulator thread

update comms (MainMenu (Just path) savePath) StartEmulator 
  = smoothTransitionAction comms
    (Emulating (Text.pack . takeBaseName $ path) True savePath "" "" Nothing Nothing) 
    (launchEmulator path comms >> (sendMsg comms . NewSaveFolder) savePath)


-- Warnings related to saving and loading

update comms s@(MainMenu Nothing _) StartEmulator
  = smoothTransition comms (Message "No ROM selected." Alert s)

update comms e@Emulating{savePath = Nothing} SaveButtonPressed
  = smoothTransition comms (chooseSaveFolderFirst e)

update comms e@Emulating{saveRomName = ""} SaveButtonPressed 
  = smoothTransition comms (Message "You need to give a name to your save file." Alert e)

update comms e@Emulating{savePath = Nothing} QuickSavePressed
  = smoothTransition comms (chooseSaveFolderFirst e)

update comms e@Emulating{saveRomName = "quick"} SaveButtonPressed 
  = smoothTransition comms (Message saveAsQuick Alert e)

update comms e@Emulating{ savePath = Nothing } QuickReloadPressed
  = smoothTransition comms (chooseSaveFolderFirst e)


-- Send commands to the SDL event loop

update comms e@Emulating{savePath = Just path} QuickSavePressed
  = Transition e (sendMsg comms (QuickSave Nothing))

update comms e@Emulating{savePath = Just path, saveRomName} SaveButtonPressed
  = Transition e (sendMsg comms (Save (path </> saveRomName <.> "purenes")))

update comms e@Emulating{ savePath = Just path } QuickReloadPressed
  = Transition e (sendMsg comms (QuickLoad Nothing))

update comms e@Emulating{} (LoadPathChanged (Just path))
  = Transition e {loadPath = path} (sendMsg comms (Load path))

update comms e@Emulating{} (SavePathChanged s)
  = Transition (e {savePath = s}) (sendMsg comms (NewSaveFolder s) >> savePathToDisk s)

update comms Emulating{savePath} ReturnToSelection 
  = smoothTransitionAction comms (MainMenu Nothing savePath) (sendMsg comms (Quit False))

update _ _ ReturnToSelection 
  = Transition (MainMenu Nothing Nothing) noop

-- Update record fields

update _ e@Emulating{} (SaveNameChanged s)
  = Transition e{saveRomName = s} noop

update _ e@Emulating{} (SaveResult res)
  = Transition e{saveResultSuccess = Just $ res} $ resultEvent res "saving"

update _ e@Emulating{} (LoadResult res)
  = Transition e{loadResultSuccess = Just $ res} $ resultEvent res "loading"

update _ m@Message{} (MessageText newMsg newIcon)
  = Transition (m {text = Text.pack newMsg, icon = newIcon}) noop


-- Pause and resume

update comms s (TogglePause shouldForward)
  = Transition 
    (updateNestedEmulationState (\ e@Emulating{isRunning = r} -> e {isRunning = not r}) s) 
    (if shouldForward then sendMsg comms (SwitchEmulationMode False) else noop)


-- Return to main menu if the SDL window was closed

update comms Emulating{savePath} SDLWindowClosed 
  = smoothTransition comms (MainMenu Nothing savePath)

update comms (Message _ _ Emulating{savePath}) SDLWindowClosed 
  = smoothTransition comms (MainMenu Nothing savePath)

update _ m@MainMenu{} SDLWindowClosed 
  = Transition m noop

update comms _ SDLWindowClosed 
  = smoothTransition comms (MainMenu Nothing Nothing)

-- Displaying messages

update comms s (MessageText msg icon) 
  = smoothTransition comms (Message (Text.pack msg) icon s)

update comms (Message _ _ stateBefore) MessageAck
  = smoothTransition comms stateBefore


-- Display error messages with a cross
-- and then return to the main menu

update comms Emulating{savePath} (Error msg) 
  = smoothTransition comms (Message (Text.pack msg) Cross (MainMenu Nothing savePath))

update comms (Message _ _ Emulating{savePath}) (Error msg) 
  = smoothTransition comms (Message (Text.pack msg) Cross (MainMenu Nothing savePath))

update comms m@MainMenu{} (Error msg)
  = smoothTransition comms (Message (Text.pack msg) Cross m)

update comms (SmoothTransition Emulating{savePath}) (Error msg)
  = smoothTransition comms (Message (Text.pack msg) Cross (MainMenu Nothing savePath))

update comms _ (Error msg) 
  = smoothTransition comms (Message (Text.pack msg) Cross (MainMenu Nothing Nothing))

-- Exit the application

update _  _ Closed 
  = Exit

-- End animation

update _ (SmoothTransition a) EndAnimation = Transition a noop

-- Ignore event if it was not handled above

update _ s _ = Transition s noop


saveFolderPersistencePath :: FilePath
saveFolderPersistencePath = "resources" </> "Emulator" </> "saveFolder" <.> "txt"


savePathToDisk :: Maybe FilePath -> IO (Maybe Event)
savePathToDisk path = only $ whenJust path (writeFile saveFolderPersistencePath)


main :: IO ()
main = do
    threadCount    <- getNumCapabilities
    gtkMessages    <- newBroadcastTChanIO
    sdlEvents      <- newChan
    let comms = CommResources { 
                toEmulatorWindow   = gtkMessages, 
                fromEmulatorWindow = sdlEvents 
              }
    let sdlWindowEventProxy = forever $ liftIO (readChan sdlEvents) >>= yield 

    previousSaveFolder <- do
      exists <- doesFileExist saveFolderPersistencePath
      if exists
      then Just <$> readFile saveFolderPersistencePath
      else return Nothing

    endAnimationWithDelay comms
    void $ run App {    view         = visualize threadCount
                      , DAS.update   = Main.update comms
                      , inputs       = [sdlWindowEventProxy]
                      , initialState = SmoothTransition (MainMenu Nothing previousSaveFolder)
                    }