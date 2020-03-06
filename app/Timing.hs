{-# LANGUAGE BangPatterns #-}

module Timing (
  uncapped,
  cappedAt,
  sleepyCappedAt
) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Loops
import Data.Word
import Data.Time.Clock.System
import SDL.Raw.Timer

uncapped :: MonadIO m => m Bool -> m ()
uncapped m = void $ iterateUntil id $ m

picoToMicro t = round (realToFrac t * 10^6)

cappedAt :: MonadIO m => m Bool -> Word32 -> m ()
cappedAt activity fps = getTicks >>= go
  where
    khz = round $ (1000 / fromIntegral fps)
    go !lastTime = do
      currentTime <- getTicks
      if currentTime > lastTime + khz
      then do
        shouldExit <- activity
        when (not shouldExit) (go currentTime)
      else 
        go lastTime

sleepyCappedAt :: MonadIO m => m Bool -> Word32 -> m ()
sleepyCappedAt activity fps = getTicks' >>= go
  where
    getTicks' = liftIO (nanoToMicro . systemNanoseconds <$> getSystemTime)
    nanoToMicro nano = round $ fromIntegral nano / 1000
    freq = round $ ((1000000 :: Double) / fromIntegral fps)
    go !lastTime = do
      currentTime <- getTicks'
      let diff = currentTime - lastTime 
      if diff < freq
      then do
        liftIO $ threadDelay (freq - diff)
        shouldExit <- activity
        afterActivity <- getTicks'
        when (not shouldExit) (go afterActivity)
      else 
        go lastTime
