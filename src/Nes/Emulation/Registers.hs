module Nes.Emulation.Registers where

import           Data.IORef.Unboxed (IORefU)
import           Data.Word

type Register   = IORefU
type Register8  = IORefU Word8
type Register16 = IORefU Word16