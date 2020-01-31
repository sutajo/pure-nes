module Communication where

import Control.Concurrent.STM.TChan
import Control.Concurrent.Chan
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