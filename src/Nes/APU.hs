module Nes.APU (
    APU(..),
    powerUp
) where

import Data.Array.IO
import Data.Array.Unboxed
import Data.Word

newtype APU = APU {
    registers  :: IOUArray Word16 Word8
}

newtype ApuSnapshot = ApuSnapshot {
    registers' :: UArray Word16 Word8
}

powerUp :: IO APU
powerUp = APU <$> newArray (0x4000, 0x4015) 0