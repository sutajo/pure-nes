module Nes.APU (
    APU(..),
    powerUp
) where

import Data.Array.IO
import Data.Word

data APU = APU {
    registers :: IOUArray Word16 Word8
}

powerUp :: IO APU
powerUp = APU <$> newArray (0x4000, 0x4015) 0