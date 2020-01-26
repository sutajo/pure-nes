module Nes.APU where

import Data.Array.IO
import Data.Word

data APU = APU {
    registers :: IOUArray Word16 Word8
}