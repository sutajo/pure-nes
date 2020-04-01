module CPU.Quick.Nestest.Spec (
  test
) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Data.Word
import           Test.Tasty
import           Test.Tasty.HUnit
import           CPU.Quick.Nestest.LogParser
import           Nes.Emulation.Monad
import           Nes.CPU.Memory (pc, p)
import           Nes.CPU.Serialization as CPU hiding (pc, p)
import           Nes.CPU.Emulation
import qualified Nes.PPU.Emulation as PPU
import           Nes.Emulation.MasterClock
import           Nes.Cartridge.INES.Parser hiding (readCartridge)


assertMatch :: (Word8, CPU, PPUState, Int) -> (Word8, CPU, PPUState) -> Assertion
assertMatch (opce, spe@CPU{irqTimer = i, nmiTimer = n}, ppue, lineNum) (opca, spa, ppua) = do
  let stateErrorMessage = "roms/tests/cpu/nestest/nestest.log:" ++ show lineNum ++":\nFailed to match the " ++ show lineNum ++ ". snapshot from the log." 
  assertEqual stateErrorMessage spe (spa {irqTimer = i, nmiTimer = n})
  assertEqual stateErrorMessage ppue ppua
  assertEqual ("Failed to match the " ++ show lineNum ++ ". opcode from the log.") opce opca 

runClock :: (Word8, CPU, PPUState, Int) -> Emulator ()
runClock expectedSnapshot = do
  op       <- fetch
  snapshot <- CPU.serialize
  cycle    <- PPU.readReg emuCycle
  scanline <- PPU.readReg emuScanLine
  liftIO $ assertMatch expectedSnapshot (op, snapshot, PPUState{..})
  clocks

test :: TestTree
test = testCase "Nestest" $ do
  contents <- BS.readFile "roms/tests/cpu/nestest/nestest.log"
  let referenceSnaps = fromRight (error "Failed to parse test log") $ A.parseOnly (logParser <* A.endOfInput) contents
  cart     <- loadCartridge "roms/tests/cpu/nestest/nestest.nes"
  nes      <- powerUpNes cart
  runEmulator nes $ do
    reset
    writeReg pc 0xC000
    writeReg p  0x24
    forM_ referenceSnaps runClock


  

  
