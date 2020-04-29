module GUI.State where

import           Data.Text (Text)
import           Communication

data State
  = MainMenu { 
      selectedRom        :: Maybe FilePath,
      previousSaveFolder :: Maybe FilePath
    }
  | ShowControls State
  | Message { 
      text              :: Text, 
      icon              :: MessageIcon, 
      stateBefore       :: State 
    }
  | Emulating { 
      romName           :: Text, 
      isRunning         :: Bool,
      savePath          :: Maybe FilePath, 
      loadPath          :: FilePath, 
      saveRomName       :: String,
      saveResultSuccess :: Maybe IOResult,
      loadResultSuccess :: Maybe IOResult
    }
  | Animation {
      targetState :: State
    }