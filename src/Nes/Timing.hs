module Nes.Timing (
  uncapped,
  cappedAt
) where

import Data.IORef
import Control.Monad
import Control.Monad.Loops
import Control.Concurrent
import Data.Time.Clock.POSIX
import Data.Time

uncapped :: (NominalDiffTime -> IO Bool) -> IO ()
uncapped m = do
  dtRef <- newIORef 0
  void $ iterateUntil id $ do
    dt <- readIORef dtRef
    before    <- getPOSIXTime
    stop      <- m dt
    after     <- getPOSIXTime
    writeIORef dtRef $! after - before
    return stop

picoToMicro t = round (realToFrac t * 10^6)

cappedAt :: NominalDiffTime -> (NominalDiffTime -> IO Bool) -> IO ()
cappedAt target activity = do
 elapsedRef <- newIORef 0
 errorRef   <- newIORef 0
 forever $ do
  elapsed <- readIORef elapsedRef
  before  <- getPOSIXTime
  stop <- activity elapsed
  after   <- getPOSIXTime
  error   <- readIORef errorRef
  let
   realElapsed = after - before
   sleep_time = target - realElapsed - error
  threadDelay (picoToMicro sleep_time)
  afterSleep  <- getPOSIXTime
  writeIORef elapsedRef $! realElapsed
  writeIORef errorRef $! (afterSleep - after) - sleep_time
  return stop