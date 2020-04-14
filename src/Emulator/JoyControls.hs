{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Emulator.JoyControls (
  JoyControlState(..),
  Emulator.JoyControls.init,
  manageButtonEvent,
  manageDeviceEvent,
  manageHatEvent,
  listJoys,
  disconnectAllJoys
) where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy
import           Control.Monad.Extra
import           Data.Int
import           Data.IORef
import           Data.Functor
import           Data.Function
import           Data.Maybe
import           Data.Word
import           Data.Text
import           Nes.Controls as Controls
import qualified Data.Vector  as V
import qualified Data.Map     as M
import           SDL.Event
import           SDL.Input.Joystick
import           SDL.Internal.Types
import           SDL.Raw.Haptic
import           SDL.Raw.Types (Haptic)
import           SDL.Raw.Event (joystickGetAttached, joystickFromInstanceID)
import           Communication


type ButtonMappings = M.Map Word8 Controls.Button


data ConnectedJoy = ConnectedJoy {
  joy :: Joystick,
  id  :: Int32,
  haptic :: Maybe Haptic
} deriving (Show)

instance Eq ConnectedJoy where
  (ConnectedJoy {id = id1}) == (ConnectedJoy {id = id2}) = id1 == id2

instance Ord ConnectedJoy where
  compare = compare `on` Emulator.JoyControls.id


data JoyControlState = JoyControlState {
  previousHatState :: IORef JoyHatPosition,
  connectedJoys    :: IORef (M.Map Int32 ConnectedJoy),
  buttonMappings   :: ButtonMappings
}


init :: ButtonMappings -> IO JoyControlState
init mappings = 
    JoyControlState <$>
    newIORef HatCentered <*>
    newIORef M.empty <*>
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


getEventJoy :: Int32 -> M.Map Int32 ConnectedJoy -> Maybe ConnectedJoy
getEventJoy searchedId map = map M.!? searchedId  


getEventJoyHaptic :: JoyControlState -> JoyButtonEventData -> IO (Maybe Haptic)
getEventJoyHaptic JoyControlState {connectedJoys} (JoyButtonEventData id _ _) = do
  joy <- readIORef connectedJoys <&> getEventJoy id
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
    if success 
    then return $ Just haptic 
    else hapticClose haptic >> return Nothing
  else return Nothing


closeHaptic :: ConnectedJoy -> IO ()
closeHaptic ConnectedJoy{..} = whenJust haptic $ hapticClose


disconnectJoy :: ConnectedJoy -> JoyControlState -> IO ()
disconnectJoy c@ConnectedJoy{joy,haptic,id}  JoyControlState{connectedJoys} = do
  putStrLn "Controller disconnected."
  modifyIORef' connectedJoys (M.delete id)
  whenJust haptic $ \h -> do
    hapticRumbleStop h
    hapticClose h
  whenM (joystickGetAttached (joystickPtr joy)) $
    closeJoystick joy


disconnectAllJoys :: JoyControlState -> IO ()
disconnectAllJoys s@JoyControlState{connectedJoys} = do
  putStrLn "Disconnecting all joysticks."
  readIORef connectedJoys >>= mapM_ (flip disconnectJoy s)


manageDeviceEvent :: JoyControlState -> JoyDeviceEventData -> IO ()
manageDeviceEvent s@JoyControlState{..} (JoyDeviceEventData conn instanceId) = do
  case conn of
    JoyDeviceAdded -> void $ runMaybeT $ do
      joyDevice <- MaybeT $ V.find (\joy -> joystickDeviceId joy == fromIntegral instanceId) <$> availableJoysticks
      liftIO $ do
        joy <- openJoystick joyDevice
        let deviceName = unpack $ joystickDeviceName joyDevice
        joyID  <- getJoystickID joy
        haptic <- initHaptic joy
        putStrLn (deviceName <> " connected" <> (if isJust haptic then " with haptic feedback support." else "."))
        modifyIORef' connectedJoys (M.insert joyID (ConnectedJoy joy joyID haptic))
    JoyDeviceRemoved -> void $ runMaybeT $ do
      connections <- liftIO $ readIORef connectedJoys
      joy   <- Joystick <$> joystickFromInstanceID instanceId
      joyID <- getJoystickID joy
      connectedJoy <- MaybeT . pure . getEventJoy joyID $ connections
      liftIO $ disconnectJoy connectedJoy s