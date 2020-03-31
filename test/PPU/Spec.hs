{-# LANGUAGE OverloadedStrings #-}

module PPU.Spec (
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
import           Nes.Emulation.Monad
import           Nes.CPU.Memory hiding (intr)
import           Nes.CPU.Emulation as CPU
import qualified Nes.PPU.Emulation as PPU
import           Nes.Emulation.MasterClock
import           Nes.Cartridge.INES.Parser hiding (readCartridge)
import           CPU.Exhaustive.Spec (runTestWith)

blargg_ppu_tests :: FilePath
blargg_ppu_tests = "roms/tests/ppu/blargg_ppu_tests/"

blargg_vbl = "roms/tests/ppu/blargg_vbl/"

blargg_vbl_nmi = "roms/tests/ppu/ppu_vbl_nmi/rom_singles/"

blargg_spriteZero = "roms/tests/ppu/ppu_sprite_hit/rom_singles/"

blargg_spriteOverflow = "roms/tests/ppu/ppu_sprite_overflow/rom_singles/"

runTest returnLoc expectedCode path romName = do
  nes        <- loadCartridge (path ++ romName) >>= powerUpNes
  runEmulator nes $ do
    CPU.reset
    PPU.reset
    replicateM_ 200 emulateFrame
    returnCode <- returnLoc
    liftIO $ expectedCode @=? returnCode

runPPUTest = runTest (PPU.read 0x20A4) 0x31 blargg_ppu_tests

runVblNmi = runTestWith clocks blargg_vbl_nmi

runSpriteZero = runTestWith clocks blargg_spriteZero

runSpriteOverflow = runTestWith clocks blargg_spriteOverflow

tests :: [TestTree]
tests =
  [
    testGroup "Memory access" [
      testCase "Palette RAM" $ runPPUTest "palette_ram.nes",
      testCase "Sprite  RAM" $ runPPUTest "sprite_ram.nes",
      testCase "VRAM"        $ runPPUTest "vram_access.nes"
    ],

    testGroup "Synchronization" [
      testCase "Vbl basics"      $ runVblNmi "01-vbl_basics.nes",
      testCase "Vbl set time"    $ runVblNmi "02-vbl_set_time.nes",
      testCase "Vbl clear time"  $ runVblNmi "03-vbl_clear_time.nes",
      testCase "Nmi control"     $ runVblNmi "04-nmi_control.nes",
      testCase "Even odd frames" $ runVblNmi "09-even_odd_frames.nes"
    ],

    testGroup "Sprite 0 Hit" [
      testCase "Basics"          $ runSpriteZero "01-basics.nes",
      testCase "Alignment"       $ runSpriteZero "02-alignment.nes",
      testCase "Corners"         $ runSpriteZero "03-corners.nes",
      testCase "Flip"            $ runSpriteZero "04-flip.nes",
      testCase "Left clip"       $ runSpriteZero "05-left_clip.nes",
      testCase "Right edge"      $ runSpriteZero "06-right_edge.nes",
      testCase "Screen bottom"   $ runSpriteZero "07-screen_bottom.nes",
      testCase "Double height"   $ runSpriteZero "08-double_height.nes",
      testCase "Timing order"    $ runSpriteZero "10-timing_order.nes"
    ],
    
    testGroup "Sprite Overflow" [
      testCase "Basics"          $ runSpriteOverflow "01-basics.nes",
      testCase "Details"         $ runSpriteOverflow "02-details.nes",
      testCase "Emulator"        $ runSpriteOverflow "05-emulator.nes"
    ]
  ]