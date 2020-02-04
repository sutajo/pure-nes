module CPU.Nestest.Spec (
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
import           CPU.Nestest.LogParser
import           Nes.EmulatorMonad
import           Nes.CPU6502
import           Nes.CPUEmulator
import           Nes.Cartridge hiding (readCartridge)


assertMatch :: (Word8, CpuSnapshot, Int) -> (Word8, CpuSnapshot) -> Assertion
assertMatch (opce, sp1, lineNum) (opca, sp2) = do
  assertEqual ("roms\\tests\\cpu\\nestest\\nestest.log:" ++ show lineNum ++":\nFailed to match the " ++ show lineNum ++ ". snapshot from the log.") sp1 sp2
  assertEqual ("Failed to match the " ++ show lineNum ++ ". opcode from the log.") opce opca 


runClock :: (Word8, CpuSnapshot, Int) -> Emulator ()
runClock expectedSnapshot = do
  op       <- fetchByte
  snapshot <- getSnapshot
  liftIO $ assertMatch expectedSnapshot (op, snapshot)
  clock

test :: TestTree
test = testCase "Nestest" $ do
  contents <- BS.readFile "roms/tests/cpu/nestest/nestest.log"
  let referenceSnaps = fromRight (error "Failed to parse test log") $ A.parseOnly (logParser <* A.endOfInput) contents
  cart     <- loadFrom "roms/tests/cpu/nestest/nestest.nes"
  nes      <- powerUpNes cart
  runEmulator nes $ do
    writeReg pc 0xC000
    writeReg p  0x24
    forM_ referenceSnaps runClock


  

  
