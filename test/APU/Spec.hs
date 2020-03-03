module APU.Spec (
    tests
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS
import           Data.Functor
import           Data.Word
import           Data.IORef
import           Prelude hiding (read)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Nes.EmulatorMonad
import           Nes.CPU6502 hiding (intr)
import           Nes.CPUEmulator as CPU
import qualified Nes.PPUEmulator as PPU
import           Nes.MasterClock
import           Nes.Cartridge hiding (readCartridge)
import           CPU.Exhaustive.Spec (runTestWith)


runMixer = runTestWith clocks "roms/tests/apu/apu_mixer/"

runApuTest = runTestWith clocks "roms/tests/apu/apu_test/rom_singles/"

tests :: [TestTree]
tests = [
    testCase "Length counter" $ runApuTest "1-len_ctr.nes",
    testCase "Length table"   $ runApuTest "2-len_table.nes",
    testCase "IRQ flag"       $ runApuTest "3-irq_flag.nes"
  ]