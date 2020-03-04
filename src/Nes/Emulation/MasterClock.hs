module Nes.Emulation.MasterClock (
    clocks,
    emulateFrame
) where

import           Control.Monad
import           Control.Monad.Loops
import           Nes.Emulation.Monad
import qualified Nes.CPU.Emulation as CPU
import qualified Nes.PPU.Emulation as PPU
import qualified Nes.APU.Emulation as APU

clocks :: Emulator ()
clocks = do
  masterClocks <- CPU.processInterrupt >> CPU.clock
  replicateM_ (masterClocks * 3) PPU.clock
  apuState <- getApu
  let (output, newApuState) = APU.clock apuState
  setApu newApuState

emulateFrame :: Emulator ()
emulateFrame = do
  initialFrameCount <- PPU.getFrameCount
  untilM_ clocks (PPU.getFrameCount <&> (/= initialFrameCount))

