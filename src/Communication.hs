{-# LANGUAGE DeriveAnyClass #-}

module Communication where

import Data.Int
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Loops
import SDL hiding (Error, Event)
import SDL.Raw.Types (Haptic)
import Nes.Controls as Controls (Input)

data Player = PlayerOne | PlayerTwo deriving (Enum)

data IOResult = IOResult { errorMsg :: Maybe String, time :: String }

data MessageIcon = Info | Alert | Cross

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
  | MessageText String MessageIcon
  | Error String
  | TogglePause { forwardToSDLWindow :: Bool }
  | QuickSavePressed
  | SaveButtonPressed
  | QuickReloadPressed
  | ShowControlsPressed
  | Noop

-- The SDL window loop executes the following commands
data Command 
  = Quit
  | Save FilePath
  | Load FilePath
  | QuickSave (Maybe Haptic)
  | QuickLoad (Maybe Haptic)
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
  | ToggleCrtShader
  | SwitchWindowMode
  | AdjustViewport Int32 Int32
  deriving (Eq) 

data CommResources = CommResources {
  toEmulatorWindow   :: TChan Command,
  fromEmulatorWindow :: Chan Event 
}

data EmulationException
  = QuickSaveNotFound
  | Other { message :: String }
  deriving (Exception)

instance Show EmulationException where
  show = \case
    QuickSaveNotFound -> unlines $ ["Could not find a quicksave file in the selected","folder."]
    Other msg -> msg

failure :: String -> IO a
failure = throw . Other

readAllTChan :: TChan a -> STM [a]
readAllTChan tchan = unfoldM (tryReadTChan tchan)

sendMessageOnException :: IO a -> CommResources -> IO a
sendMessageOnException op comms = op `catch` \(e :: SomeException) -> do
  writeChan (fromEmulatorWindow comms) (Error $ show e)
  throw e