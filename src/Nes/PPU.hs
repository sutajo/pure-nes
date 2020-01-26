module Nes.PPU (
    PPU(..)
) where

import Data.Array.IO
import Data.Word

data PPU = PPU {
    registers :: IOUArray Word16 Word8
}