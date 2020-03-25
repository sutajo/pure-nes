{-# LANGUAGE BangPatterns #-}

module Timing (
  uncapped,
  cappedAt,
  sleepyCappedAt
) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Data.Word
import Data.Time.Clock.System
import SDL.Raw.Timer

uncapped :: MonadIO m => m Bool -> m ()
uncapped m = getTicks >>= go (0 :: Double) 0
 where
   go !dt !frameCount !lastTime = do
    exit         <- m
    currentTime  <- getTicks
    let (newDt, newFrameCount) = if dt > 1.0 then (0, 0) else (fromIntegral (currentTime - lastTime) / 1000 + dt, frameCount+1)
    liftIO $ when (newDt == 0) $ putStrLn $ "FPS: " ++ show frameCount
    when (not exit) $ go newDt newFrameCount currentTime

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
      shouldExit  <- activity
      let diff = currentTime - lastTime 
      if diff < freq
      then do
        liftIO $ threadDelay (freq - diff)
        afterActivity <- getTicks'
        when (not shouldExit) (go afterActivity)
      else 
        go lastTime
