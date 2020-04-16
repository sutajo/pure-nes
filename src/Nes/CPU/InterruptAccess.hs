module Nes.CPU.InterruptAccess where

import Nes.Emulation.Registers

data InterruptAccess = InterruptAccess {
  irq        :: Register8,
  nmi        :: Register8
}