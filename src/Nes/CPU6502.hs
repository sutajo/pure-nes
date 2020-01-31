{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}

module Nes.CPU6502 (
  CPU(..),
  Flag(..),
  Interrupt(..),
  Instruction(..),
  CpuSnapshot(..),
  powerUp
)where

import Data.Word  (Word8, Word16)
import Data.IORef.Unboxed (IORefU, newIORefU)

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

powerUp :: IO CPU
powerUp = do
  p <- newIORefU 0x34
  a <- newIORefU 0
  x <- newIORefU 0
  y <- newIORefU 0
  s <- newIORefU 0xFD
  pc <- newIORefU 0
  cyc  <- newIORefU 0
  intr <- newIORefU 0
  return CPU{..} 

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

-- Official instructions
data Instruction
 = ADC | AND | ASL | BCC | BCS | BEQ | BIT
 | BMI | BNE | BPL | BRK | BVC | BVS | CLC
 | CLD | CLI | CLV | CMP | CPX | CPY | DEC
 | DEX | DEY | EOR | INC | INX | INY | JMP
 | JSR | LDA | LDX | LDY | LSR | NOP | ORA
 | PHA | PHP | PLA | PLP | ROL | ROR | RTI
 | RTS | SBC | SEC | SED | SEI | STA | STX
 | STY | TAX | TAY | TSX | TXA | TXS | TYA
 deriving (Show)


data CpuSnapshot = CpuSnapshot {
  sa     ::  Word8, 
  sx     ::  Word8, 
  sy     ::  Word8, 
  spc    ::  Word16,
  ss     ::  Word8, 
  sp     ::  Word8, 
  scyc   ::  Int
} deriving (Eq, Show)