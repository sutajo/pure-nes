module Nes.Emulation.MasterClock (
    syncCPUwithPPU,
    emulateFrame,
    resetNes
) where

import           Control.Monad
import           Control.Monad.Loops
import           Nes.Emulation.Monad
import           Nes.CPU.Memory
import qualified Nes.CPU.Emulation as CPU
import qualified Nes.PPU.Emulation as PPU
import qualified Nes.APU.Emulation as APU



syncCPUwithPPU :: Emulator CPU ()
syncCPUwithPPU = do
  clocks <- CPU.processInterrupts >> CPU.runNextInstruction
  directPPUAccess $ replicateM_ (clocks * 3) PPU.clock
  {-
  replicateM_ clocks $ do
    apuState <- getApu
    let (output, newApuState) = APU.clock apuState clocks
    setApu newApuState
  -}

emulateFrame :: Emulator Nes FrameBuffer
emulateFrame = do
  initialFrameCount <- emulatePPU PPU.getFrameCount
  emulateCPU $ untilM_ syncCPUwithPPU (directPPUAccess PPU.getFrameCount <&> (/= initialFrameCount))
  emulatePPU PPU.accessScreen


resetNes :: Emulator Nes ()
resetNes = do
  emulateCPU CPU.reset
  emulatePPU PPU.reset 
