{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.Emulation.Monad (
    Nes(..),
    Emulator,
    powerUpNes,
    runEmulator,
    emulateSubcomponent,
    emulateCPU,
    emulatePPU,
    accessPPU,
    createPPUAccess,
    directPPUAccess,
    getMirroring,
    useMemory,
    readMemory,
    writeMemory,
    readReg,
    writeReg,
    modifyReg,
    getApu,
    setApu,
    readCartridgeWithAccessor,
    writeCartridgeWithAccessor,
    sendPendingNmi,
    sendPendingIrq,
    sendNmi,
    sendIrq,
    readController,
    writeController,
    processInput,
    module Data.Functor,
    module Data.Primitive,
    module PPU,
    module Control.Monad.Reader
) where

import           Control.Monad.Reader
import qualified Data.Vector.Unboxed.Mutable  as VUM
import           Data.Primitive(Prim)
import qualified Data.Vector.Mutable as VM
import           Data.Mutable
import           Data.Functor
import           Nes.Emulation.Registers
import           Nes.APU.Memory         as APU
import           Nes.CPU.Memory         as CPU
import           Nes.PPU.Memory         as PPU
import qualified Nes.Cartridge.Memory   as Cart
import qualified Nes.Controls           as Controls

data Nes =  Nes {
    cpu         ::  CPU,
    ppu         ::  PPU,
    apu         ::  IORef APU,
    cartridge   ::  Cart.Cartridge
}

powerUpNes :: Cart.Cartridge -> IO Nes
powerUpNes cart = do
  (cpu, ppu) <- CPU.powerUp cart

  Nes cpu ppu            <$>
    newIORef APU.powerUp <*>
    pure cart


newtype Emulator component value = Emulator (ReaderT component IO value)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader component)


runEmulator :: c -> Emulator c a -> IO a
runEmulator component (Emulator emu) = runReaderT emu component

emulateSubcomponent :: (b -> c) -> Emulator c a -> Emulator b a
emulateSubcomponent component (Emulator emu) = Emulator (withReaderT component emu)

emulateCPU :: Emulator CPU a -> Emulator Nes a
emulateCPU = emulateSubcomponent cpu

emulatePPU :: Emulator PPU a -> Emulator Nes a
emulatePPU = emulateSubcomponent ppu

accessPPU :: Emulator PPUAccess a -> Emulator CPU a
accessPPU = emulateSubcomponent ppuAccess

createPPUAccess :: Emulator PPU a -> Emulator PPUAccess a
createPPUAccess = emulateSubcomponent useAccess

directPPUAccess = accessPPU . createPPUAccess

-- Memory access

useMemory :: (component -> reference) -> (reference -> IO value) -> Emulator component value
useMemory memory action = asks memory >>= liftIO . action

readMemory :: Enum addr => (c -> VUM.IOVector Word8) -> addr -> Emulator c Word8
readMemory comp addr = useMemory comp (`VUM.unsafeRead` fromEnum addr)

writeMemory :: Enum addr => (c -> VUM.IOVector Word8) -> addr -> Word8 -> Emulator c ()
writeMemory comp addr val = useMemory comp (\arr -> VUM.unsafeWrite arr (fromEnum addr) val)

readReg :: Prim a => (component -> Register a) -> Emulator component a
readReg = flip useMemory readIORefU

writeReg :: Prim a => (component -> Register a) -> a -> Emulator component ()
writeReg reg val = useMemory reg (flip writeIORefU val)

modifyReg :: Prim a => (component -> Register a) -> (a -> a) -> Emulator component ()
modifyReg reg f = readReg reg >>= writeReg reg . f

readCartridgeWithAccessor :: (component -> Cart.CartridgeAccess) -> Word16 -> Emulator component Word8
readCartridgeWithAccessor selector addr = do
  reader <- asks (Cart.readCartridge . selector)
  liftIO $ reader addr

writeCartridgeWithAccessor :: (component -> Cart.CartridgeAccess) -> Word16 -> Word8 -> Emulator component ()
writeCartridgeWithAccessor selector addr val = do
  writer <- asks (Cart.writeCartridge . selector)
  liftIO $ writer addr val

getMirroring :: (component -> Cart.CartridgeAccess) -> Emulator component (Word16 -> Word16)
getMirroring selector = do
  getmirroring <- asks (Cart.getMirroring . selector)
  liftIO getmirroring

getApu :: Emulator Nes APU
getApu = useMemory apu readIORef

setApu :: APU -> Emulator Nes ()
setApu val = useMemory apu (`writeIORef` val)

-- Cpu interrupts

sendPendingInterrupt :: (InterruptRegisters -> Register8) -> Word8 -> (component -> InterruptRegisters) -> Emulator component ()
sendPendingInterrupt register pendingCycles component =
  useMemory (register . component) $ \reg -> reg `writeIORefU` pendingCycles

sendPendingNmi :: Word8 -> (component -> InterruptRegisters) -> Emulator component ()
sendPendingNmi = sendPendingInterrupt nmi

sendNmi :: (component -> InterruptRegisters) -> Emulator component ()
sendNmi = sendPendingNmi 1

sendPendingIrq :: Word8 -> (component -> InterruptRegisters) -> Emulator component ()
sendPendingIrq = sendPendingInterrupt irq

sendIrq :: (component -> InterruptRegisters) -> Emulator component ()
sendIrq = sendPendingIrq 1

-- Controller access

readController :: Int -> Emulator CPU Word8
readController index =
  useMemory controllers $ \cs -> do
    controller <- VM.read cs index
    let (byte, newControllerState) = Controls.read controller
    VM.write cs index newControllerState
    pure byte

modifyController action index =
  useMemory controllers $ \cs -> do
    VM.modify cs action index

writeController byte = modifyController (Controls.write byte)

processInput input = modifyController (Controls.processInput input)

