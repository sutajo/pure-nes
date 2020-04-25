{-# LANGUAGE OverloadedStrings #-}

module CPU.Exhaustive.Spec (
    tests,
    runTestWith
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
import           Nes.Emulation.MasterClock
import           Nes.CPU.Memory hiding (intr)
import           Nes.CPU.Emulation as CPU
import qualified Nes.PPU.Emulation as PPU
import           Nes.Cartridge.INES.Parser hiding (readCartridge)

instr_v5 :: FilePath
instr_v5 = "roms/tests/cpu/instr_test-v5/"

misc :: FilePath
misc = "roms/tests/cpu/instr_misc/rom_singles/"

intr :: FilePath
intr = "roms/tests/cpu/cpu_interrupts_v2/rom_singles/"

instr_time :: FilePath
instr_time = "roms/tests/cpu/instr_timing/"      

runTestWith :: Emulator Nes a -> FilePath -> String -> Assertion
runTestWith stepper path romName = do
  nes        <- loadCartridge (path ++ romName) >>= powerUpNes
  runEmulator nes $ do
    resetNes
    sanityCheck <- do 
      emulateCPU $ write 0x6000 0x80
      untilM_ stepper (emulateCPU $ read 0x6000 <&> (<0x80))
      emulateCPU $ forM [0x6001..0x6003] read
    liftIO $ zipWithM_ (@?=) sanityCheck [0xDE, 0xB0, 0x61] -- make sure the test results are valid
    emulateCPU $ do
      returnCode <- read 0x6000
      msg  <- readNullTerminatedString 0x6004
      liftIO $ assertEqual (msg ++ "\nTest exitcode indicates failure.") 0 returnCode

runTest :: FilePath -> String -> Assertion
runTest = runTestWith (emulateCPU runNextInstruction) 

tests :: [TestTree]
tests =
  [
    testGroup "InstrTest_v5"
    [
      testCase "Basics"     $ runTest instr_v5 "rom_singles/01-basics.nes",
      testCase "Implied"    $ runTest instr_v5 "rom_singles/02-implied.nes",
      testCase "Immediate"  $ runTest instr_v5 "rom_singles/03-immediate.nes",
      testCase "ZeroPage"   $ runTest instr_v5 "rom_singles/04-zero_page.nes",
      testCase "ZeroPageXY" $ runTest instr_v5 "rom_singles/05-zp_xy.nes",
      testCase "Absolute"   $ runTest instr_v5 "rom_singles/06-absolute.nes",
      testCase "AbsoluteXY" $ runTest instr_v5 "rom_singles/07-abs_xy.nes",
      testCase "IndX"       $ runTest instr_v5 "rom_singles/08-ind_x.nes",
      testCase "IndY"       $ runTest instr_v5 "rom_singles/09-ind_y.nes",
      testCase "Branch"     $ runTest instr_v5 "rom_singles/10-branches.nes",
      testCase "Stack"      $ runTest instr_v5 "rom_singles/11-stack.nes",
      testCase "JMP-JSR"    $ runTest instr_v5 "rom_singles/12-jmp_jsr.nes",
      testCase "RTS"        $ runTest instr_v5 "rom_singles/13-rts.nes",
      testCase "RTI"        $ runTest instr_v5 "rom_singles/14-rti.nes",
      testCase "BRK"        $ runTest instr_v5 "rom_singles/15-brk.nes",
      testCase "Special"    $ runTest instr_v5 "rom_singles/16-special.nes"
    ],

    testGroup "Instr_Misc"
    [
      testCase "Abs X wrap"  $ runTest misc "01-abs_x_wrap.nes",
      testCase "Branch wrap" $ runTest misc "02-branch_wrap.nes"
    ],

    testGroup "Instr_Timing" $
    [
      --testCase "Branch" $ runTest instr_time "rom_singles/2-branch_timing.nes"
    ],
    
    testGroup "Interrupts" $ 
    [
      --testCase "Cli latency" $ runTest intr "1-cli_latency.nes"
    ]
  ]