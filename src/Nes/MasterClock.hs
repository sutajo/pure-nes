module Nes.MasterClock (
    clocks,
    emulateFrame
) where

import           Control.Monad
import           Control.Monad.Loops
import           Nes.EmulatorMonad
import qualified Nes.CPUEmulator as CPU
import qualified Nes.PPUEmulator as PPU

clocks :: Emulator ()
clocks = do
  masterClocks <- CPU.clock
  replicateM_ (masterClocks * 3) PPU.clock
  CPU.processInterrupt

emulateFrame :: Emulator ()
emulateFrame = do
  initialFrameCount <- PPU.getFrameCount
  untilM_ clocks (PPU.getFrameCount <&> (/= initialFrameCount))

