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
import qualified Data.Vector as V
import qualified Data.Map as M
import           SDL.Event
import           SDL.Input.Joystick
import           SDL.Input.GameController()

data JoyControlState = JoyControlState {
  previousHatState :: IORef JoyHatPosition,
  connectedJoys    :: IORef (V.Vector Joystick),
  buttonMappings   :: M.Map Word8 Button
}

init :: IO JoyControlState
init = 
  JoyControlState      <$>
  newIORef HatCentered <*>
  newIORef V.empty     <*>
  pure M.empty

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
    getJoysWithID :: IO (V.Vector (Joystick, Int32))
    getJoysWithID = readIORef connectedJoys >>= V.mapM (\joy -> (,) <$> pure joy <*> getJoystickID joy)
    getEventJoy :: V.Vector (Joystick, Int32) -> Maybe Joystick 
    getEventJoy = fmap fst . V.find (\(_,joyID) -> joyID == id)
  case conn of
    JoyDeviceAdded -> void $ runMaybeT $ do
      joyDevice <- MaybeT $ V.find (\joy -> joystickDeviceId joy == fromIntegral id) <$> availableJoysticks
      liftIO $ do
        joy <- openJoystick joyDevice
        modifyIORef' connectedJoys (V.cons joy) 
    JoyDeviceRemoved -> void $ runMaybeT $ do
      joysWithID <- liftIO $ getJoysWithID
      eventJoy  <- MaybeT . pure . getEventJoy $ joysWithID
      liftIO $ do
        writeIORef connectedJoys (V.map fst . V.filter ((/= id) . snd) $ joysWithID)
        closeJoystick eventJoy 