module Nes.Emulation.MasterClock (
    execCpuInstruction,
    emulateFrame
) where

import           Control.Monad
import           Control.Monad.Loops
import           Nes.Emulation.Monad
import qualified Nes.CPU.Emulation as CPU
import qualified Nes.PPU.Emulation as PPU
import qualified Nes.APU.Emulation as APU

execCpuInstruction :: Emulator ()
execCpuInstruction = do
  masterClocks <- CPU.processInterrupt >> CPU.clock
  replicateM_ (masterClocks * 3) PPU.clock
  {-
  replicateM_ masterclocks $ do
    apuState <- getApu
    let (output, newApuState) = APU.clock apuState masterClocks
    setApu newApuState
  -}

emulateFrame :: Emulator FrameBuffer
emulateFrame = do
  initialFrameCount <- PPU.getFrameCount
  untilM_ execCpuInstruction (PPU.getFrameCount <&> (/= initialFrameCount))
  PPU.accessScreen

