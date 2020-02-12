{-# LANGUAGE OverloadedStrings #-}

module PPU.Exhaustive.Spec where

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

blargg_ppu_tests :: FilePath
blargg_ppu_tests = "roms/tests/ppu/blargg_ppu_tests/"

runTest :: FilePath -> String -> Assertion
runTest path romName = do
  nes        <- loadCartridge (path ++ romName) >>= powerUpNes
  runEmulator nes $ do
    reset
    write 0x6000 0x80
    untilM_ clock (read 0x6000 <&> (<0x80))
    sanityNumbers <- forM [0x6001..0x6003] read
    liftIO $ zipWithM_ (@?=) sanityNumbers [0xDE, 0xB0, 0x61] -- make sure test results are valid
    returnCode <- read 0x6000
    msg  <- readNullTerminatedString 0x6004
    liftIO $ assertEqual (msg ++ "\nTest exitcode indicates failure.") 0 returnCode

tests :: [TestTree]
tests =
  [
    --testCase "Palette RAM"      $ runTest blargg_ppu_tests "palette_ram.nes",
    --testCase "Powerup Palette"  $ runTest blargg_ppu_tests "power_up_palette.nes"
  ]