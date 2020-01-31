module CPU.Nestest.Spec (
  test
) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import           Data.Either
import           Data.Word
import           Test.Tasty
import           Test.Tasty.HUnit
import           CPU.Nestest.LogParser
import           Nes.EmulatorMonad
import           Nes.CPU6502
import           Nes.CPUEmulator
import           Nes.Cartridge


assertMatch :: (Word8, CpuSnapshot, Int) -> (Word8, CpuSnapshot) -> Assertion
assertMatch (opce, sp1, lineNum) (opca, sp2) = do
  assertEqual ("Failed to match the " ++ show lineNum ++ ". opcode from the log.") opce opca
  print opce
  assertEqual ("Failed to match the " ++ show lineNum ++ ". snapshot from the log.") sp1 sp2 


runTestROM :: [(Word8, CpuSnapshot, Int)] -> Emulator ()
runTestROM [] = pure ()
runTestROM (x:xs) = do
  op       <- fetchOpcode
  pc       <- fetch
  clock
  snapshot <- getSnapshot
  liftIO $ assertMatch x (op, snapshot { spc = pc })
  runTestROM xs


test :: TestTree
test = testCase "CPU" $ do
  contents <- BS.readFile "roms/tests/cpu/nestest/nestest.log"
  let referenceSnaps = fromRight (error "Failed to parse test log") $ A.parseOnly (logParser <* A.endOfInput) contents
  cart     <- loadFrom "roms/tests/cpu/nestest/nestest.nes"
  nes      <- powerUpNes cart
  runEmulator nes $ do
    writeReg pc 0xC000
    runTestROM referenceSnaps


  

  
