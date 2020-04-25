import Control.DeepSeq
import Control.Monad.Loops
import Criterion.Main
import Criterion.Types
import Data.Time
import qualified System.CPU as Info
import Nes.Cartridge.INES.Parser
import Nes.Emulation.Monad
import Nes.Emulation.MasterClock
import Nes.CPU.Emulation as CPU
import Nes.PPU.Emulation as PPU

instance NFData Nes where
  rnf x = x `seq` ()

loadNesFrom :: FilePath -> IO Nes
loadNesFrom path = loadCartridge path >>= powerUpNes 

runBench :: String -> Benchmark
runBench name = bench name $ perRunEnv resource computation
    where
      resource = loadNesFrom ("bench/benchROMs/" ++ name)
      computation nes = do
        runEmulator nes $ do
          resetNes
          untilM_ (emulateCPU syncCPUwithPPU) (emulatePPU isRenderingEnabled)

main :: IO ()
main = do
  time  <- getCurrentTime
  (cpu:_) <- Info.getCPUs
  let Just modelName = Info.modelName cpu
  let formattedTime = formatTime defaultTimeLocale "%m_%d_%H_%M" time
  defaultMainWith 
    defaultConfig { reportFile = Just ("bench/results/report_" ++ formattedTime ++ "_" ++ show modelName ++ ".html") } $
      [
        runBench "factorial.nes",
        runBench "delay10s.nes",
        runBench "delay20s.nes"
      ]