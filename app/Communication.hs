module Communication where

import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Loops
import SDL hiding (Error, Event)
import Nes.Controls as Controls (Input)

data Event 
  = FileSelectionChanged (Maybe FilePath) -- A new file was selected in the main menu
  | SavePathChanged (Maybe FilePath)      -- A new save folder was selected
  | LoadPathChanged (Maybe FilePath)      -- New file selected to be loaded
  | SaveError (Maybe String, String)      -- Save results
  | LoadError (Maybe String, String)      -- Load results
  | SaveNameChanged String                -- Name of unique save changed
  | Closed 
  | Help 
  | StartEmulator 
  | ControllerConfig
  | MessageAck
  | CloseEmulator
  | SDLWindowClosed
  | MessageText String
  | Error String
  | SwitchMode { sentFromSDLWindow :: Bool }
  | QuickSavePressed
  | SaveButtonPressed
  | QuickReloadPressed

data ParentMessage 
  = Stop
  | Switch
  | TraceRequest
  | SaveVM FilePath
  | LoadVM FilePath
  | NewSaveFolder (Maybe FilePath)

data Command 
  = Quit
  | Save FilePath
  | Load FilePath
  | QuickSave
  | QuickLoad
  | SaveFolder (Maybe FilePath)
  | PlayerInput Input 
  | SwitchEmulationMode { sendEventToGtkWindow :: Bool } -- switches between continous and step-by-step emulation
  | JoyButtonCommand JoyButtonEventData
  | JoyDeviceCommand JoyDeviceEventData
  | JoyHatCommand JoyHatEventData
  | StepOneFrame
  | StepClockCycle
  | ToggleJoyMap
  deriving (Eq)

data CommResources = CommResources {
  toSDLWindow   :: TChan ParentMessage,
  fromSDLWindow :: Chan Event 
}

readAllTChan :: TChan a -> STM [a]
readAllTChan tchan = unfoldM (tryReadTChan tchan)

except op (comms, msg) = op `onException` do
  writeChan (fromSDLWindow comms) (Error msg)