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


sendMsg :: CommResources -> Command -> IO (Maybe Event)
sendMsg CommResources{toEmulatorWindow} = only . atomically . writeTChan toEmulatorWindow

-- | If a field should change while displaying a message
--   then update the nested emulation state
updateNestedEmulationState :: (State -> State) -> State -> State
updateNestedEmulationState f e@Message{stateBefore = s} = e{stateBefore = updateNestedEmulationState f s }
updateNestedEmulationState f e@Emulating{} = f e
updateNestedEmulationState f x = x

-- | GUI state transition function
update :: CommResources -> State -> Event -> Transition State Event

-- New filepath selected in the main menu

update _ s@Started{} (FileSelectionChanged p) 
  = Transition s{selectedRom = p} (return Nothing)


-- Show controls and then return if Ok was pressed

update _ s@Started{} ShowControlsPressed = Transition (ShowControls s) noop

update _ (ShowControls s) MessageAck = Transition s noop


-- Start the emulator thread

update comms (Started (Just path) savePath) StartEmulator 
  = Transition 
    (Emulating (Text.pack . takeBaseName $ path) True savePath "" "" Nothing Nothing) 
    (only $ launchEmulator path comms >> (sendMsg comms . NewSaveFolder) savePath)


-- Warnings related to saving and loading

update _ s@(Started Nothing _) StartEmulator
  = Transition s (return . Just $ MessageText "No ROM selected." Alert)

update _ e@Emulating{savePath = Nothing} SaveButtonPressed
  = Transition e (emit $ chooseSaveFolderFirst)

update _ e@Emulating{saveRomName = ""} SaveButtonPressed 
  = Transition e (emit $ MessageText "You need to give a name to your save file." Alert)

update _ e@Emulating{savePath = Nothing} QuickSavePressed
  = Transition e (emit $ chooseSaveFolderFirst)

update _ e@Emulating{saveRomName = "quick"} SaveButtonPressed 
  = Transition e (emit $ MessageText saveAsQuick Alert)

update _ e@Emulating{ savePath = Nothing } QuickReloadPressed
  = Transition e (emit $ chooseSaveFolderFirst)


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
  = Transition (Started Nothing savePath) (sendMsg comms Quit)


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

update _ Emulating{savePath} SDLWindowClosed 
  = Transition (Started Nothing savePath) noop


-- Displaying messages

update _ s (MessageText msg icon) 
  = Transition (Message (Text.pack msg) icon s) noop

update comms (Message _ _ stateBefore) MessageAck 
  = Transition stateBefore noop


-- Display error messages with a cross
-- and then return to the main menu

update _ Emulating{savePath} (Error msg) 
  = Transition (Message (Text.pack msg) Cross (Started Nothing savePath)) noop


update _ _ (Error msg) 
  = Transition (Message (Text.pack msg) Cross (Started Nothing Nothing)) noop

-- Exit the application

update _  _ Closed 
  = Exit


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

    void $ run App {    view         = visualize threadCount
                      , DAS.update   = Main.update comms
                      , inputs       = [sdlWindowEventProxy]
                      , initialState = Started Nothing previousSaveFolder
                    }