{-# LANGUAGE RecordWildCards #-}

module JoyControls (
  JoyControlState(..),
  JoyControls.init,
  manageButtonEvent,
  manageDeviceEvent,
  manageHatEvent
) where

import           Control.Monad(void)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy
import           Data.Int
import           Data.IORef
import           Data.Functor()
import           Data.Maybe (fromMaybe)
import           Data.Word
import           Nes.Controls as Controls
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Map as M
import           SDL.Event
import           SDL.Input.Joystick
import           SDL.Input.GameController()


type ButtonMappings = M.Map Word8 Controls.Button


data ConnectedJoy = ConnectedJoy {
  joy :: Joystick,
  id  :: Int32
} deriving (Show)

instance Eq ConnectedJoy where
  (ConnectedJoy _ id1) == (ConnectedJoy _ id2) = id1 == id2


data JoyControlState = JoyControlState {
  previousHatState :: IORef JoyHatPosition,
  connectedJoys    :: IORef [ConnectedJoy],
  buttonMappings   :: ButtonMappings
}


init :: ButtonMappings -> IO JoyControlState
init mappings = 
    JoyControlState <$>
    newIORef HatCentered <*>
    newIORef [] <*>
    pure mappings

convertHatState :: JoyHatPosition -> Button
convertHatState hatPos = case hatPos of
  HatUp       -> Up
  HatDown     -> Down
  HatLeft     -> Controls.Left
  HatRight    -> Controls.Right
  _           -> error "HatState not convertible"

manageButtonEvent :: JoyControlState -> JoyButtonEventData -> IO [UserInput]
manageButtonEvent JoyControlState{..} (JoyButtonEventData _ btn state) = do
  let 
    controllerButton = fromMaybe Select (buttonMappings M.!? btn)
    action = case state of
      JoyButtonPressed  -> Press
      JoyButtonReleased -> Release
  pure [action controllerButton]


manageHatEvent :: JoyControlState -> JoyHatEventData -> IO [UserInput]
manageHatEvent JoyControlState{..} (JoyHatEventData _ _ newState) = execWriterT $ do
  when (newState `elem` [HatUp, HatDown, HatLeft, HatRight, HatCentered]) $ do
    prevHat <- liftIO $ readIORef previousHatState
    if prevHat `elem` [HatUp, HatDown, HatLeft, HatRight]
    then tell [Release $ convertHatState prevHat]
    else pure ()
    liftIO $ writeIORef previousHatState newState
    when (newState /= HatCentered) $ do 
      tell [Press $ convertHatState newState]


manageDeviceEvent :: JoyControlState -> JoyDeviceEventData -> IO ()
manageDeviceEvent JoyControlState{..} (JoyDeviceEventData conn id) = do
  let
    getEventJoy :: [ConnectedJoy] -> Maybe ConnectedJoy 
    getEventJoy = L.find (\(ConnectedJoy _ joyId) -> joyId == id)
  case conn of
    JoyDeviceAdded -> void $ runMaybeT $ do
      joyDevice <- MaybeT $ V.find (\joy -> joystickDeviceId joy == fromIntegral id) <$> availableJoysticks
      liftIO $ do
        joy <- openJoystick joyDevice
        modifyIORef' connectedJoys (ConnectedJoy joy id :) 
    JoyDeviceRemoved -> void $ runMaybeT $ do
      connections <- liftIO $ readIORef connectedJoys
      ConnectedJoy eventJoy  _ <- MaybeT . pure . getEventJoy $ connections
      liftIO $ do
        modifyIORef' connectedJoys (L.deleteBy (==) (ConnectedJoy eventJoy id))
        closeJoystick eventJoy 