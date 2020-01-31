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
powerUp = APU <$> newArray (4000, 4017) 0