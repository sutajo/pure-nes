module Nes.PPUEmulator where

import Data.Array.IO
import Data.Word
import Nes.PPU
import Nes.EmulatorMonad

readPPU :: Word16 -> Emulator Word8
readPPU addr = useMemory (registers . ppu) $ (flip readArray addr)

writePPU :: Word16 -> Word8 -> Emulator ()
writePPU addr val = useMemory (registers . ppu) $ (\arr -> writeArray arr addr val)

