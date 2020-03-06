{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, FlexibleInstances #-}

import Control.DeepSeq
import Control.Monad.Loops
import Criterion.Main
import Criterion.Types
import Data.Vector.Mutable
import Data.Time.Clock
import GHC.Generics
import Nes.Controls
import Nes.Cartridge.Parser
import Nes.Emulation.Monad
import Nes.Emulation.MasterClock
import Nes.CPU.Memory
import Nes.CPU.Emulation as CPU
import Nes.PPU.Emulation as PPU

instance NFData (IOVector Controller) where
  rnf x = x `seq` ()

instance NFData CPU where
  rnf x = x `seq` ()

instance NFData PPU where
  rnf x = x `seq` ()

deriving instance Generic Nes
deriving instance NFData Nes

loadNesFrom :: FilePath -> IO Nes
loadNesFrom path = loadCartridge path >>= powerUpNes 

runBench :: String -> Benchmark
runBench name = bench name $ perRunEnv resource computation
    where
      resource = loadNesFrom ("bench/benchROMs/" ++ name)
      computation nes = do
        runEmulator nes $ do
          CPU.reset >> PPU.reset
          untilM_ clocks isRenderingEnabled

main :: IO ()
main = do
  time <- getCurrentTime
  defaultMainWith 
    defaultConfig { reportFile = Just ("bench/results/report_" ++ show time ++ ".html") } $
      [
        runBench "factorial.nes",
        runBench "delay10s.nes",
        runBench "delay20s.nes"
      ]