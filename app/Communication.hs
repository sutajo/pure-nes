module Communication where

import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Loops
import SDL hiding (Error, Event)
import Nes.Controls as Controls (Input)

type ControllerId = Int

data IOResult = IOResult { errorMsg :: Maybe String, time :: String }

data Event 
  = FileSelectionChanged (Maybe FilePath) -- A new file was selected in the main menu
  | SavePathChanged (Maybe FilePath)      -- A new save folder was selected
  | LoadPathChanged (Maybe FilePath)      -- New file was selected to be loaded
  | SaveResult IOResult
  | LoadResult IOResult
  | SaveNameChanged String                -- Name of unique save changed
  | Closed                                -- Gtk window was closed
  | StartEmulator                         -- Start button was pressed in main menu
  | MessageAck                            -- User acknowledged message
  | ReturnToSelection
  | SDLWindowClosed
  | MessageText String
  | Error String
  | SwitchMode { forwardToSDLWindow :: Bool }
  | QuickSavePressed
  | SaveButtonPressed
  | QuickReloadPressed

-- The SDL window loop executes the following commands
data Command 
  = Quit
  | Save FilePath
  | Load FilePath
  | QuickSave
  | QuickLoad
  | NewSaveFolder (Maybe FilePath)
  | PlayerOneInput Input
  | PlayerTwoInput Input
  | SwitchEmulationMode { forwardToGtkWindow :: Bool }
  | JoyButtonCommand JoyButtonEventData
  | JoyDeviceCommand JoyDeviceEventData
  | JoyHatCommand JoyHatEventData
  | StepOneFrame
  | StepClockCycle
  | ToggleJoyMap
  deriving (Eq) 

data CommResources = CommResources {
  toSDLWindow   :: TChan Command,
  fromSDLWindow :: Chan Event 
}

readAllTChan :: TChan a -> STM [a]
readAllTChan tchan = unfoldM (tryReadTChan tchan)

except op (comms, msg) = op `onException` do
  writeChan (fromSDLWindow comms) (Error msg)