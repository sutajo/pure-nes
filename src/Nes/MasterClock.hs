module Nes.MasterClock (
    clocks
) where

import           Control.Monad
import           Nes.EmulatorMonad
import qualified Nes.CPUEmulator as CPU
import qualified Nes.PPUEmulator as PPU

clocks :: Emulator ()
clocks = do
  masterClocks <- CPU.clock
  replicateM_ (masterClocks * 3) PPU.clock

