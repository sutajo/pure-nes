module Nes.Timing (
  uncapped,
  cappedAt
) where

import Control.Monad.IO.Class
import Data.IORef
import Control.Monad
import Control.Monad.Loops
import Control.Concurrent
import Data.Time.Clock.POSIX
import Data.Time

uncapped :: MonadIO m => (NominalDiffTime -> m Bool) -> m ()
uncapped m = do
  dtRef <- liftIO $ newIORef 0
  void $ iterateUntil id $ do
    dt <- liftIO $ readIORef dtRef
    before    <- liftIO $ getPOSIXTime
    stop      <- m dt
    liftIO $ do
      after   <- getPOSIXTime
      writeIORef dtRef $! after - before
    return stop

picoToMicro t = round (realToFrac t * 10^6)

cappedAt :: MonadIO m => (NominalDiffTime -> m Bool) -> NominalDiffTime -> m ()
cappedAt activity hz = do
 let target = 1 / hz
 elapsedRef <- liftIO $ newIORef 0
 errorRef   <- liftIO $ newIORef 0
 void $ iterateUntil id $ do
  elapsed <- liftIO $ readIORef elapsedRef
  before  <- liftIO $ getPOSIXTime
  stop <- activity elapsed
  liftIO $ do
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