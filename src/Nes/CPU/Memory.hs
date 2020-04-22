{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, DeriveAnyClass #-}

module Nes.CPU.Memory (
  CPU(..),
  Flag(..),
  Interrupt(..),
  InterruptAccess(..),
  RAM,
  Register8,
  Register16,
  Nes.CPU.Memory.powerUp
) where


import           Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable  as VUM
import           Nes.CPU.InterruptAccess
import           Nes.PPU.Memory as PPUMEM
import           Nes.Cartridge.Memory
import qualified Nes.Controls           as Controls


type RAM = VUM.IOVector Word8

type Register8  = IORefU Word8
type Register16 = IORefU Word16

allocateRAM :: IO RAM
allocateRAM = VUM.new 0x800


-- http://obelisk.me.uk/6502/reference.html
data CPU = CPU {
  -- Registers
  a          ::  Register8,      -- accumulator
  x          ::  Register8,      -- index
  y          ::  Register8,      -- index
  pc         ::  Register16,     -- program counter
  s          ::  Register8,      -- stack pointer
  p          ::  Register8,      -- status register
  cyc        ::  IORefU Int,     -- elapsed cycles


  interrupts      ::  InterruptAccess,
  ram             ::  RAM,
  ppuAccess       ::  PPUAccess,
  cartridgeAccess ::  CartridgeAccess,
  controllers     ::  IOVector Controls.Controller 
}

powerUp :: Cartridge -> IO (CPU, PPU)
powerUp cart = do
  p <- newIORefU 0x34
  a <- newIORefU 0
  x <- newIORefU 0
  y <- newIORefU 0
  s <- newIORefU 0xFD
  pc <- newIORefU 0
  cyc  <- newIORefU 0
  irq <- newIORefU 0
  nmi <- newIORefU 0
  let interrupts = InterruptAccess{..}
  ram <- allocateRAM
  ppu <- PPUMEM.powerUp interrupts cart
  let cartridgeAccess = getCPUAccess cart
  let ppuAccess = PPUAccess ppu
  controllers <- VM.replicate 2 Controls.powerUp
  return (CPU{..}, ppu)

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

data Interrupt
  = NMI
  | IRQ