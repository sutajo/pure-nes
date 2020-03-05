module Communication where

import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Loops

data Event 
  = FileSelectionChanged (Maybe FilePath)
  | SavePathChanged (Maybe FilePath)
  | LoadPathChanged (Maybe FilePath)
  | SaveError (Maybe String)
  | LoadError (Maybe String)
  | SaveNameChanged String
  | Closed 
  | Help 
  | StartEmulator 
  | ControllerConfig
  | MessageAck
  | CloseEmulator
  | SDLWindowClosed
  | MessageText String
  | Error String
  | SwitchMode
  | QuickSavePressed
  | SaveButtonPressed
  | QuickReloadPressed

data ParentMessage 
  = Stop
  | Switch
  | TraceRequest
  | SaveVM FilePath
  | LoadVM FilePath

data CommResources = CommResources {
  toSDLWindow   :: TChan ParentMessage,
  fromSDLWindow :: Chan Event 
}

readAllTChan :: TChan a -> STM [a]
readAllTChan tchan = unfoldM (tryReadTChan tchan)

except op (comms, msg) = op `onException` do
  writeChan (fromSDLWindow comms) (Error msg)