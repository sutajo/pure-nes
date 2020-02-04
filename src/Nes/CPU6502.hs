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
  cyc  <- newIORefU 7 -- https://forums.nesdev.com/viewtopic.php?f=3&t=14231
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
  a'     ::  Word8, 
  x'     ::  Word8, 
  y'     ::  Word8, 
  pc'    ::  Word16,
  s'     ::  Word8, 
  p'     ::  Word8, 
  cyc'   ::  Int
} deriving (Eq, Show)