{-# LANGUAGE OverloadedStrings #-}

module PPU.Exhaustive.Spec (
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
import           System.Directory
import           Test.Tasty
import           Test.Tasty.HUnit
import           Nes.EmulatorMonad
import           Nes.CPU6502 hiding (intr)
import           Nes.CPUEmulator as CPU
import qualified Nes.PPUEmulator as PPU
import           Nes.MasterClock
import           Nes.Cartridge hiding (readCartridge)
import           CPU.Exhaustive.Spec (runTestWith)

blargg_ppu_tests :: FilePath
blargg_ppu_tests = "roms/tests/ppu/blargg_ppu_tests/"

blargg_vbl = "roms/tests/ppu/blargg_vbl/"

blargg_vbl_nmi = "roms/tests/ppu/ppu_vbl_nmi/rom_singles/"

runTest returnLoc expectedCode path romName = do
  nes        <- loadCartridge (path ++ romName) >>= powerUpNes
  runEmulator nes $ do
    CPU.reset
    PPU.reset
    replicateM_ 300 emulateFrame
    returnCode <- returnLoc
    liftIO $ expectedCode @=? returnCode

runPPUTest = runTest (PPU.read 0x20A4) 0x31 blargg_ppu_tests
runVbl     = runTest (PPU.read 0x20C2) 0x50 blargg_vbl

runVblNmi = runTestWith clocks blargg_vbl_nmi

tests :: [TestTree]
tests =
  [
    testGroup "Memory access" [
      testCase "Palette RAM" $ runPPUTest "palette_ram.nes",
      testCase "VRAM"        $ runPPUTest "vram_access.nes"
    ],

    testGroup "Synchronization" [
      testCase "Frame basics"    $ runVbl    "1.frame_basics.nes",
      testCase "Vbl basics"      $ runVblNmi "01-vbl_basics.nes",
      testCase "Vbl set time"    $ runVblNmi "02-vbl_set_time.nes",
      testCase "Vbl clear time"  $ runVblNmi "03-vbl_clear_time.nes",
      testCase "Nmi control"     $ runVblNmi "04-nmi_control.nes",
      --testCase "Nmi timing"      $ runVblNmi "05-nmi_timing.nes",
      --testCase "Suppression"     $ runVblNmi "06-suppression.nes",
      --testCase "Nmi on timing"   $ runVblNmi "07-nmi_on_timing.nes",
      --testCase "Nmi off timing"  $ runVblNmi "08-nmi_off_timing.nes",
      testCase "Even odd frames" $ runVblNmi "09-even_odd_frames.nes"
      --testCase "Even odd timing" $ runVblNmi "10-even_odd_timing.nes"
    ]
  ]