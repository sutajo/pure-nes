module Nes.Emulation.MasterClock (
    execCpuInstruction,
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



execCpuInstruction :: Emulator CPU ()
execCpuInstruction = do
  masterClocks <- CPU.processInterrupts >> CPU.clock
  directPPUAccess $ replicateM_ (masterClocks * 3) PPU.clock
  {-
  replicateM_ masterclocks $ do
    apuState <- getApu
    let (output, newApuState) = APU.clock apuState masterClocks
    setApu newApuState
  -}

emulateFrame :: Emulator Nes FrameBuffer
emulateFrame = do
  initialFrameCount <- emulatePPU PPU.getFrameCount
  emulateCPU $ untilM_ execCpuInstruction (directPPUAccess PPU.getFrameCount <&> (/= initialFrameCount))
  emulatePPU PPU.accessScreen


resetNes :: Emulator Nes ()
resetNes = do
  emulateCPU CPU.reset
  emulatePPU PPU.reset 
