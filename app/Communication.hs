module Communication where

import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Loops

data Event 
  = FileSelectionChanged (Maybe FilePath) 
  | Closed 
  | Help 
  | StartEmulator 
  | ControllerConfig
  | MessageAck
  | CloseEmulator
  | SDLWindowClosed
  | ErrorReport String
  | SwitchMode
  | SaveButtonPressed
  | LoadButtonPressed

data ParentMessage 
  = Stop
  | Switch
  | TraceRequest

data CommResources = CommResources {
  toSDLWindow   :: TChan ParentMessage,
  fromSDLWindow :: Chan Event 
}

readAllTChan :: TChan a -> STM [a]
readAllTChan tchan = unfoldM (tryReadTChan tchan)

except op (comms, msg) = op `onException` do
  writeChan (fromSDLWindow comms) (ErrorReport msg)