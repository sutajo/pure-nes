{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.Emulation.Monad (
    Nes(..),
    Emulator,
    powerUpNes,
    runEmulator,
    useMemory,
    readMemory,
    writeMemory,
    getApu,
    setApu,
    cpuReadCartridge,
    cpuWriteCartridge,
    ppuReadCartridge,
    ppuWriteCartridge,
    getNametableMirroring,
    sendPendingNmi,
    sendPendingIrq,
    sendNmi,
    clearNmi,
    sendIrq,
    useCpu,
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
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM
import           Data.Functor
import           Nes.APU.Memory         as APU
import           Nes.CPU.Memory         as CPU
import           Nes.PPU.Memory         as PPU
import qualified Nes.Cartridge.Parser   as Cart
import qualified Nes.Controls           as Controls

type RAM = VUM.IOVector Word8

allocateRAM :: IO RAM
allocateRAM = VUM.new 0x800

data Nes =  Nes {
    cpu         ::  CPU,
    ram         ::  RAM,
    ppu         ::  PPU,
    apu         ::  IORef APU,
    cartridge   ::  Cart.Cartridge,
    controllers ::  IOVector Controls.Controller
}

powerUpNes :: Cart.Cartridge -> IO Nes
powerUpNes cart = 
    Nes         <$>
    CPU.powerUp <*>
    allocateRAM <*>
    PPU.powerUp (Cart.mirror cart) <*>
    newIORef APU.powerUp <*>
    pure cart            <*>
    VM.replicate 2 Controls.powerUp

newtype Emulator a = Emulator (ReaderT Nes IO a) 
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Nes, MonadFail)


runEmulator :: Nes -> Emulator a -> IO a
runEmulator nes (Emulator emu) = runReaderT emu nes

-- Memory access

useMemory :: (Nes -> b) -> (b -> IO a) -> Emulator a
useMemory memory action = ask >>= liftIO . action . memory
{-# INLINE useMemory #-}

readMemory :: Enum addr => (Nes -> VUM.IOVector Word8) -> addr -> Emulator Word8
readMemory comp addr = useMemory comp $ (`VUM.read` (fromEnum addr))

writeMemory :: Enum addr => (Nes -> VUM.IOVector Word8) -> addr -> Word8 -> Emulator ()
writeMemory comp addr val = useMemory comp $ (\arr -> VUM.write arr (fromEnum addr) val)

readCartridgeWith accessor addr = useMemory cartridge (`accessor` addr)
writeCartridgeWith modifier addr val = useMemory cartridge $ \cart -> modifier cart addr val

cpuReadCartridge :: Word16 -> Emulator Word8
cpuReadCartridge = readCartridgeWith Cart.cpuReadCartridge 

cpuWriteCartridge :: Word16 -> Word8 -> Emulator ()
cpuWriteCartridge = writeCartridgeWith Cart.cpuWriteCartridge

ppuReadCartridge :: Word16 -> Emulator Word8
ppuReadCartridge = readCartridgeWith Cart.ppuReadCartridge

ppuWriteCartridge :: Word16 -> Word8 -> Emulator ()
ppuWriteCartridge = writeCartridgeWith Cart.ppuWriteCartridge

getNametableMirroring :: Emulator (Word16 -> Word16)
getNametableMirroring = ask <&> (mirrorNametableAddress . ppu)

useCpu :: (b -> IO a) -> (CPU -> b) -> Emulator a
useCpu action field = useMemory (field . cpu) action

getApu :: Emulator APU
getApu = useMemory apu readIORef

setApu :: APU -> Emulator ()
setApu val = useMemory apu (`writeIORef` val)

-- Cpu interrupts

-- Sends an NMI which fires after the specified amount of clock cycles
sendPendingInterrupt :: (CPU -> Register8) -> Word8 -> Emulator ()
sendPendingInterrupt reg cyc = useCpu (`writeIORefU` cyc) reg

clearInterrupt :: (CPU -> Register8) -> Emulator ()
clearInterrupt reg = useCpu (`writeIORefU` 0) reg

sendPendingNmi = sendPendingInterrupt nmiTimer
sendNmi  = sendPendingNmi 1
clearNmi = clearInterrupt nmiTimer
sendPendingIrq = sendPendingInterrupt irqTimer
sendIrq = sendPendingIrq 1

-- Controller access

readController :: Int -> Emulator Word8
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
    