module Main where


import           Control.Monad                  
import           Control.Monad.IO.Class
import           Control.Concurrent       hiding (yield)
import           Control.Concurrent.STM
import qualified Data.Text                     as Text
import           GI.Gtk.Declarative.App.Simple as DAS
import           System.FilePath
import           Pipes
import           Emulator.Window
import           GUI.Window
import           GUI.State
import           Communication


-- | Launch the emulator on a new, bound thread
launchEmulator :: FilePath -> CommResources -> IO ()
launchEmulator path comms = do
  toSDLWindow' <- atomically $ dupTChan (toSDLWindow comms)
  void . forkOS $ runEmulatorWindow path comms { toSDLWindow = toSDLWindow' }


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
sendMsg CommResources{toSDLWindow} = only . atomically . writeTChan toSDLWindow


-- | GUI state transition function
update :: CommResources -> State -> Event -> Transition State Event

-- New filepath selected in the main menu

update _ (Started _) (FileSelectionChanged p) 
  = Transition (Started p) (return Nothing)


-- Show controls and then return if Ok was pressed

update _ (Started _) ShowControlsPressed = Transition ShowControls noop

update _ ShowControls MessageAck = Transition (Started Nothing) noop


-- Start the emulator thread

update comms s@(Started (Just path)) StartEmulator 
  = Transition 
    (Emulating (Text.pack . takeBaseName $ path) True Nothing "" "" Nothing Nothing) 
    (only $ launchEmulator path comms)


-- Warnings related to saving and loading

update _ s@(Started Nothing) StartEmulator
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

update comms e (LoadPathChanged (Just path))
  = Transition e {loadPath = path} (sendMsg comms (Load path))

update comms e (SavePathChanged s)
  = Transition (e {savePath = s}) (sendMsg comms (NewSaveFolder s))

update comms Emulating{} ReturnToSelection 
  = Transition (Started Nothing) (sendMsg comms Quit)


-- Update record fields

update _ e (SaveNameChanged s)
  = Transition e{saveRomName = s} noop

update _ e (SaveResult res)
  = Transition e{saveResultSuccess = Just $ res} $ resultEvent res "saving"

update _ e (LoadResult res)
  = Transition e{loadResultSuccess = Just $ res} $ resultEvent res "loading"

update _ m@Message{} (MessageText newMsg newIcon)
  = Transition (m {text = Text.pack newMsg, icon = newIcon}) noop


-- Pause and resume

update comms e (SwitchMode shouldForward)
  = Transition 
    (e { running = not (running e) }) 
    (if shouldForward then sendMsg comms (SwitchEmulationMode False) else noop)


-- Return to main menu if the SDL window was closed

update _ _ SDLWindowClosed 
  = Transition (Started Nothing) noop


-- Displaying messages

update _ s (MessageText msg icon) 
  = Transition (Message (Text.pack msg) icon s) noop

update comms (Message _ _ stateBefore) MessageAck 
  = Transition stateBefore (sendMsg comms (NewSaveFolder Nothing))


-- Display error messages with a cross
-- and then return to the main menu

update _ s (Error msg) 
  = Transition (Message (Text.pack msg) Cross (Started Nothing)) noop


-- Exit the application

update _  _ Closed 
  = Exit


-- Ignore event if it was not handled above

update _ s _ = Transition s noop




main :: IO ()
main = do
    threadCount    <- getNumCapabilities
    gtkMessages    <- newBroadcastTChanIO
    sdlEvents      <- newChan
    let comms = CommResources { 
                toSDLWindow   = gtkMessages, 
                fromSDLWindow = sdlEvents 
              }
    let sdlWindowEventProxy = forever $ liftIO (readChan sdlEvents) >>= yield 
    void $ run App {    view         = visualize threadCount
                      , DAS.update   = Main.update comms
                      , inputs       = [sdlWindowEventProxy]
                      , initialState = Started Nothing
                    }