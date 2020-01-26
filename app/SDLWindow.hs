{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, GADTs #-}

module SDLWindow where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Loops
import qualified Data.Vector.Storable.Mutable as V
import           Data.Word
import           Data.Int
import           Data.IORef
import           System.Exit
import           SDL
import           SDL.Audio
import           SDL.Input.Joystick
import           Nes.Cartridge

data ParentMessage 
  = Stop
  | TraceRequest

data ChildMessage
  = Trace Int

scale :: Int
scale = 4

width :: Int
width = 256

height :: Int
height = 240

data Intent = Quit | KeyPress SDL.Keysym deriving (Eq)

processEvent :: SDL.EventPayload -> IO [Intent]
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ k)) = print (SDL.keysymKeycode k) >> return [KeyPress k]
processEvent SDL.QuitEvent = return [Quit]
processEvent _ = pure []

processParentMessage :: ParentMessage -> IO [Intent]
processParentMessage Stop = pure [Quit]
processParentMessage TraceRequest = print "TraceRequest" >> pure []

greetings :: IO ()
greetings = do
  putStrLn "Starting Pure-Nes Emulator."
  putStr "SDL Version: "
  print =<< SDL.version
  putStr "Detected audio devices:"
  print =<< SDL.getAudioDeviceNames ForPlayback

bye = putStrLn "Emulator closed successfully."

sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n / 44100 :: Double
             freq = 440 * 2 * pi
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (t * freq)))
      [0 :: Int32 ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do samples' <- readIORef samples
         let n = V.length buffer
         zipWithM_ (V.write buffer)
                   [0 ..]
                   (take n samples')
         writeIORef samples
                    (drop n samples')
    _ -> error "Unsupported audio format"

initAudio :: IO AudioDevice
initAudio = do
  samples <- newIORef sinSamples
  let spec = OpenDeviceSpec
            {
                openDeviceFreq = Mandate $ 44100
              , openDeviceFormat = Mandate $ Signed16BitNativeAudio
              , openDeviceChannels = Mandate $ Mono
              , openDeviceSamples = 4096 * 2
              , openDeviceCallback = audioCB samples
              , openDeviceUsage = ForPlayback
              , openDeviceName = Nothing
            }
  fst <$> openAudioDevice spec

readAllTchan :: TChan a -> STM [a]
readAllTchan tchan =  do
  content <- tryReadTChan tchan
  case content of
    Nothing -> return []
    Just a  -> (a:) <$> readAllTchan tchan

runEmulator :: FilePath -> TChan ParentMessage -> IO ()
runEmulator romPath msgpipe = do
  Nes.Cartridge.initFrom romPath
  SDL.initializeAll
  device <- initAudio
  setAudioDevicePlaybackState device Play
  greetings
  let windowConfig = SDL.defaultWindow {
    windowInitialSize = V2 (fromIntegral $ width * scale) (fromIntegral $ height * scale)
  }
  window <- SDL.createWindow "Pure-Nes Emulator" windowConfig
  void $ iterateUntil (elem Quit) $ do
    threadDelay (10^4)
    sdlIntents <- fmap concat $ mapM (processEvent . SDL.eventPayload) =<< SDL.pollEvents
    gtkIntents <- fmap concat $ mapM processParentMessage =<< (atomically $ readAllTchan msgpipe)
    return (gtkIntents ++ sdlIntents)
  SDL.closeAudioDevice device
  SDL.quit
  bye