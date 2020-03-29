{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module JoyControls (
  JoyControlState(..),
  JoyControls.init,
  manageButtonEvent,
  manageDeviceEvent,
  manageHatEvent,
  listJoys
) where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy
import           Control.Monad.Extra
import           Data.Int
import           Data.IORef
import           Data.Functor
import           Data.Maybe
import           Data.Word
import           Nes.Controls as Controls
import qualified Data.List    as L
import qualified Data.Vector  as V
import qualified Data.Map     as M
import           SDL.Event
import           SDL.Input.Joystick
import           SDL.Internal.Types
import           SDL.Raw.Haptic
import           SDL.Raw.Types (Haptic)
import           Communication


type ButtonMappings = M.Map Word8 Controls.Button


data ConnectedJoy = ConnectedJoy {
  joy :: Joystick,
  id  :: Int32,
  haptic :: Maybe Haptic
} deriving (Show)

instance Eq ConnectedJoy where
  (ConnectedJoy {id = id1}) == (ConnectedJoy {id = id2}) = id1 == id2


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

listJoys :: JoyControlState -> IO ()
listJoys JoyControlState{..} = do
  putStr "Available joys:"
  availableJoysticks >>= print
  putStr "Opened joysticks:"
  readIORef connectedJoys >>= print 

convertHatState :: JoyHatPosition -> Button
convertHatState hatPos = case hatPos of
  HatUp       -> Up
  HatDown     -> Down
  HatLeft     -> Controls.Left
  HatRight    -> Controls.Right
  _           -> error "HatState not convertible"

getEventJoy :: Int32 -> [ConnectedJoy] -> Maybe ConnectedJoy
getEventJoy searchedId = L.find (\(ConnectedJoy { id = joyId }) -> joyId == searchedId)

getEventJoyHaptic :: JoyControlState -> JoyButtonEventData -> IO (Maybe Haptic)
getEventJoyHaptic JoyControlState { connectedJoys = joys } (JoyButtonEventData id _ _) = do
  joy <- readIORef joys <&> getEventJoy id
  return $ joy >>= haptic

manageButtonEvent :: ControllerId -> JoyControlState -> JoyButtonEventData -> IO (Maybe Command)
manageButtonEvent _ s e@(JoyButtonEventData _ 4 JoyButtonPressed) = getEventJoyHaptic s e <&> Just . QuickSave
manageButtonEvent _ s e@(JoyButtonEventData _ 6 JoyButtonPressed) = getEventJoyHaptic s e <&> Just . QuickLoad
manageButtonEvent cid JoyControlState{..} (JoyButtonEventData _ btn state) =
  let 
    command = case cid of
      0 -> PlayerOneInput
      1 -> PlayerTwoInput
      _ -> error "Invalid controller id"
    mappedButton = buttonMappings M.!? btn
    action = case state of
      JoyButtonPressed  -> Press
      JoyButtonReleased -> Release
  in return $ do
    button <- mappedButton
    return $ command (action button)


manageHatEvent :: JoyControlState -> JoyHatEventData -> IO [Input]
manageHatEvent JoyControlState{..} (JoyHatEventData _ _ newState) = execWriterT $ do
  when (newState `elem` [HatUp, HatDown, HatLeft, HatRight, HatCentered]) $ do
    prevHat <- liftIO $ readIORef previousHatState
    if prevHat /= HatCentered
    then tell [Release $ convertHatState prevHat]
    else pure ()
    liftIO $ writeIORef previousHatState newState
    when (newState /= HatCentered) $ do 
      tell [Press $ convertHatState newState]

initHaptic :: Joystick -> IO (Maybe Haptic)
initHaptic joy = do
  let ptr = joystickPtr joy
  let joyIsHaptic = fmap (1==) . joystickIsHaptic
  isHaptic <- joyIsHaptic ptr
  if isHaptic then do
    haptic <- hapticOpenFromJoystick ptr 
    success <- hapticRumbleInit haptic <&> ( == 0)
    return $
      if success 
      then Just haptic 
      else Nothing
  else return Nothing

closeHaptic :: ConnectedJoy -> IO ()
closeHaptic ConnectedJoy{..} = whenJust haptic $ hapticClose

manageDeviceEvent :: JoyControlState -> JoyDeviceEventData -> IO ()
manageDeviceEvent JoyControlState{..} (JoyDeviceEventData conn id) = do
  case conn of
    JoyDeviceAdded -> void $ runMaybeT $ do
      joyDevice <- MaybeT $ V.find (\joy -> joystickDeviceId joy == fromIntegral id) <$> availableJoysticks
      liftIO $ do
        joy <- openJoystick joyDevice
        haptic <- initHaptic joy
        putStrLn ("Controller" <> (if isJust haptic then " (with haptic feedback support)" else "") <> " connected.")
        modifyIORef' connectedJoys (ConnectedJoy joy id haptic :) 
    JoyDeviceRemoved -> void $ runMaybeT $ do
      connections <- liftIO $ readIORef connectedJoys
      c@ConnectedJoy { joy, haptic }  <- MaybeT . pure . getEventJoy id $ connections
      liftIO $ do
        putStrLn "Controller disconnected."
        modifyIORef' connectedJoys (L.delete c)
        whenJust haptic $ hapticClose
        closeJoystick joy 