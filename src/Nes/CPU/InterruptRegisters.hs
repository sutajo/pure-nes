module Nes.CPU.InterruptRegisters where

import Nes.Emulation.Registers

data InterruptRegisters = InterruptRegisters {
  irq        :: Register8,
  nmi        :: Register8
}
