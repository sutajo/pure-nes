module Communication where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.Chan
import Control.Exception
import Data.IORef

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

data ParentMessage 
  = Stop
  | TraceRequest

data CommResources = CommResources {
  toSDLWindow   :: TChan ParentMessage,
  fromSDLWindow :: Chan Event 
}

readAllTChan :: TChan a -> STM [a]
readAllTChan tchan =  do
  content <- tryReadTChan tchan
  case content of
    Nothing -> return []
    Just a  -> (a:) <$> readAllTChan tchan


except op (comms, msg) = op `onException` do
  writeChan (fromSDLWindow comms) (ErrorReport msg)