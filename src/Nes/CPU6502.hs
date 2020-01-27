{-# LANGUAGE MultiParamTypeClasses #-}

module Nes.CPU6502 (
  CPU(..),
  Flag(..),
  Interrupt(..),
)where

import Data.Word  (Word8, Word16)
import Data.IORef.Unboxed (IORefU)

-- http://obelisk.me.uk/6502/reference.html
data CPU = CPU {
  a     ::  IORefU Word8,  -- accumulator
  x     ::  IORefU Word8,  -- index
  y     ::  IORefU Word8,  -- index
  pc    ::  IORefU Word16, -- program counter
  s     ::  IORefU Word8,  -- stack pointer
  p     ::  IORefU Word8,  -- status register
  cyc   ::  IORefU Int,    -- elapsed cycles
  intr  ::  IORefU Word8   -- interrupt register
}

data Interrupt
  = NONE
  | NMI
  | IRQ
  deriving (Enum)

data Flag 
  = Carry
  | Zero
  | InterruptDisable
  | DecimalMode
  | BreakCommand
  | Unused
  | Overflow
  | Negative 
  deriving (Enum)