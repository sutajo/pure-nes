{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, DeriveAnyClass #-}

module Nes.CPU6502 (
  CPU(..),
  Flag(..),
  CpuSnapshot(..),
  Register8,
  Register16,
  powerUp
)where

import Data.Word  (Word8, Word16)
import Data.Store
import GHC.Generics
import Data.IORef.Unboxed (IORefU, newIORefU)
import Text.Printf

type Register8  = IORefU Word8
type Register16 = IORefU Word16

-- http://obelisk.me.uk/6502/reference.html
data CPU = CPU {
  a          ::  Register8,      -- accumulator
  x          ::  Register8,      -- index
  y          ::  Register8,      -- index
  pc         ::  Register16,     -- program counter
  s          ::  Register8,      -- stack pointer
  p          ::  Register8,      -- status register
  cyc        ::  IORefU Int,     -- elapsed cycles
  irqTimer   ::  Register8,
  nmiTimer   ::  Register8
}

powerUp :: IO CPU
powerUp = do
  p <- newIORefU 0x34
  a <- newIORefU 0
  x <- newIORefU 0
  y <- newIORefU 0
  s <- newIORefU 0xFD
  pc <- newIORefU 0
  cyc  <- newIORefU 0
  irqTimer <- newIORefU 0
  nmiTimer <- newIORefU 0
  return CPU{..}

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

data CpuSnapshot = CpuSnapshot {
  a'     ::  Word8, 
  x'     ::  Word8, 
  y'     ::  Word8, 
  pc'    ::  Word16,
  s'     ::  Word8, 
  p'     ::  Word8, 
  cyc'   ::  Int,
  irqTimer' :: Word8,
  nmiTimer' :: Word8
} deriving (Eq, Generic, Store)

instance Show CpuSnapshot where
  show CpuSnapshot{..} = printf "Cpu { a = 0x%X, x = 0x%X, y = 0x%X, pc = 0x%X, s = 0x%X, p = 0x%X, cycles = %d }" a' x' y' pc' s' p' cyc'