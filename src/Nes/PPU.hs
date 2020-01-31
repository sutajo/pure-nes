module Nes.PPU (
    PPU(..),
    powerUp
) where

import Data.Array.IO
import Data.Word

data PPU = PPU {
    registers :: IOUArray Word16 Word8
}

powerUp :: IO PPU
powerUp = PPU <$> newArray (2000, 2007) 0