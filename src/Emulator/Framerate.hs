{-# LANGUAGE BangPatterns #-}

module Emulator.Framerate (
  uncapped,
  cappedAt,
  sleepyCappedAt
) where

import Control.Monad.IO.Class
import Control.Monad
import Data.Word
import SDL.Raw.Timer
import SDL (delay)


uncapped :: MonadIO m => m Bool -> m ()
uncapped m = getTicks >>= go (0 :: Double) 0
 where
   go !dt !frameCount !lastTime = do
    exit         <- m
    currentTime  <- getTicks
    let (newDt, newFrameCount) = if dt > 1.0 then (0, 0) else (fromIntegral (currentTime - lastTime) / 1000 + dt, frameCount+1)
    liftIO $ when (newDt == 0) $ putStrLn $ "FPS: " ++ show frameCount
    when (not exit) $ go newDt newFrameCount currentTime


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
sleepyCappedAt activity fps = getTicks >>= go
  where
    khz = round $ (1000 / fromIntegral fps)
    go !lastTime = do
      shouldExit  <- activity
      currentTime <- getTicks
      let diff = currentTime - lastTime 
      when (diff < khz) $ do
        SDL.delay (khz - diff)
      when (not shouldExit) (go (lastTime + khz))
