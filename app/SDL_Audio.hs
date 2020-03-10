{-# LANGUAGE GADTs #-}

module SDL_Audio (
  AudioResources(..),
  acquireAudioResources,
  releaseAudioResources,
  play,
  module Nes.APU.Oscillator
) where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.Int
import qualified Data.Vector.Storable.Mutable as VM
import           Control.Exception
import           SDL
import           Nes.APU.Oscillator

data AudioResources = AudioResources {
  device :: AudioDevice
}

sampleOscillator :: Oscillator Float -> [Int16]
sampleOscillator (Oscillator f) =
  map (\n ->
         let t = fromIntegral n / 44100 :: Float
             freq = 200 * 2 * pi
         in round (fromIntegral (maxBound `div` 2 :: Int16) * f (t * freq)))
      [0 :: Int32 ..]

audioCallBack :: IORef [Int16] -> AudioFormat sampleType -> VM.IOVector sampleType -> IO ()
audioCallBack samples format buffer =
  case format of
    Signed16BitLEAudio -> void.forkIO $
      do samples' <- readIORef samples
         let n = VM.length buffer
         zipWithM_ (VM.write buffer)
                   [0 ..]
                   (take n samples')
         writeIORef samples
                    (drop n samples')
    _ -> error "Unsupported audio format"

acquireAudioResources oscillator = do
  SDL.initialize [InitAudio]
  samples <- newIORef (sampleOscillator oscillator)
  let 
    spec = OpenDeviceSpec
      {
          openDeviceFreq     = Desire 44100
        , openDeviceFormat   = Desire Signed16BitNativeAudio
        , openDeviceChannels = Desire Mono
        , openDeviceSamples  = 4096
        , openDeviceCallback = audioCallBack samples
        , openDeviceUsage    = ForPlayback
        , openDeviceName     = Nothing
      }
  (device, _) <- SDL.openAudioDevice spec
  return AudioResources{..}

releaseAudioResources AudioResources{..} = do
  SDL.closeAudioDevice device
  SDL.quit

play :: Oscillator Float -> IO ()
play o@(Oscillator f) = bracket (acquireAudioResources o) releaseAudioResources playAudio
  where
    playAudio AudioResources{..} = do
      setAudioDevicePlaybackState device Play
      forever $ threadDelay (10^6)