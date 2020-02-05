{-# LANGUAGE OverloadedStrings #-}

module CPU.Instr_test_v5.Spec (
    test
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
import           Nes.CPUEmulator
import           Nes.Cartridge hiding (readCartridge)

v5 :: FilePath
v5 = "roms/tests/cpu/instr_test-v5/"

misc :: FilePath
misc = "roms/tests/cpu/instr_misc/rom_singles/"

intr :: FilePath
intr = "roms/tests/cpu/cpu_interrupts_v2/rom_singles/"

-- Read null terminated string
readNullTerminatedString :: Word16 -> Emulator String
readNullTerminatedString addr = do
  addrRef <- liftIO $ newIORef addr
  let readByte = liftIO (readIORef addrRef) >>= read
  untilM 
    (do readByte <* liftIO (modifyIORef' addrRef (+1)))
      (readByte <&> (== 0)) <&> (map (toEnum.fromEnum))

runTest :: FilePath -> String -> Assertion
runTest path romName = do
  nes        <- loadFrom (path ++ romName) >>= powerUpNes
  (resultCode, msg) <- runEmulator nes $ do
    reset
    write 0x6000 0x80
    untilM_ clock (read 0x6000 <&> (<0x80))
    code <- read 0x6000
    msg  <- readNullTerminatedString 0x6004
    pure (code, msg)
  assertEqual (msg ++ "\nTest exitcode indicates failure.") 0 resultCode

test :: TestTree
test =
  testGroup "InstructionTests" $
  [
    testGroup "InstrTest_v5" $ 
    [
      testCase "Basics"     $ runTest v5 "rom_singles/01-basics.nes",
      testCase "Implied"    $ runTest v5 "rom_singles/02-implied.nes",
      testCase "Immediate"  $ runTest v5 "rom_singles/03-immediate.nes",
      testCase "ZeroPage"   $ runTest v5 "rom_singles/04-zero_page.nes",
      testCase "ZeroPageXY" $ runTest v5 "rom_singles/05-zp_xy.nes",
      testCase "Absolute"   $ runTest v5 "rom_singles/06-absolute.nes",
      testCase "AbsoluteXY" $ runTest v5 "rom_singles/07-abs_xy.nes",
      testCase "IndX"       $ runTest v5 "rom_singles/08-ind_x.nes",
      testCase "IndY"       $ runTest v5 "rom_singles/09-ind_y.nes",
      testCase "Branch"     $ runTest v5 "rom_singles/10-branches.nes",
      testCase "Stack"      $ runTest v5 "rom_singles/11-stack.nes",
      testCase "JMP-JSR"    $ runTest v5 "rom_singles/12-jmp_jsr.nes",
      testCase "RTS"        $ runTest v5 "rom_singles/13-rts.nes",
      testCase "RTI"        $ runTest v5 "rom_singles/14-rti.nes",
      testCase "BRK"        $ runTest v5 "rom_singles/15-brk.nes",
      testCase "Special"    $ runTest v5 "rom_singles/16-special.nes"
    ],

    testGroup "Instr_Misc" $
    [
      testCase "Abs X wrap"  $ runTest misc "01-abs_x_wrap.nes",
      testCase "Branch wrap" $ runTest misc "02-branch_wrap.nes"
    ],

    testGroup "Interrupts" $ 
    [
      testCase "Cli latency" $ runTest intr "1-cli_latency.nes"
    ]
  ]