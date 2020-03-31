module GUI.State where

import           Data.Text (Text)
import           Communication

data State
  = Started (Maybe FilePath)
  | Message { text :: Text, icon :: MessageIcon, stateAfterOk :: State }
  | ShowControls
  | Emulating { 
      romName :: Text, 
      running :: Bool,
      savePath :: Maybe FilePath, 
      loadPath :: FilePath, 
      saveRomName :: String,
      saveResultSuccess :: Maybe IOResult,
      loadResultSuccess :: Maybe IOResult
    }