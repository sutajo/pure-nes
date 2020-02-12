{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.EmulatorMonad (
    Nes(..),
    Emulator,
    powerUpNes,
    runEmulator,
    useMemory,
    cpuReadCartridge,
    cpuWriteCartridge,
    ppuReadCartridge,
    ppuWriteCartridge,
    getNametableMirroring,
    sendInterrupt,
    useCpu
) where

import           Control.Monad.Reader
import           Control.Monad.Fail
import           Data.Array.IO
import           Data.Word (Word8, Word16)
import           Data.IORef.Unboxed
import           Data.Primitive(Prim)
import           Data.Functor
import           Data.Bits
import           Nes.CPU6502   as CPU
import           Nes.PPU       as PPU
import qualified Nes.Cartridge as Cart
import           Nes.APU       as APU

type RAM = IOUArray Word16 Word8

allocateRAM :: IO RAM
allocateRAM = newArray (0, 0x07FF) 0

data Nes =  Nes {
    cpu         ::  CPU,
    ram         ::  RAM,
    ppu         ::  PPU,
    apu         ::  APU,
    cartridge   ::  Cart.Cartridge
}

powerUpNes :: Cart.Cartridge -> IO Nes
powerUpNes cart = 
    Nes         <$>
    CPU.powerUp <*>
    allocateRAM <*>
    PPU.powerUp (Cart.mirror cart) <*>
    APU.powerUp <*>
    pure cart


newtype Emulator a = Emulator { 
    unEmulator :: ReaderT Nes IO a 
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Nes, MonadFail)


runEmulator :: Nes -> Emulator a -> IO a
runEmulator nes (Emulator emu) = runReaderT emu nes

useMemory :: (Nes -> b) -> (b -> IO a) -> Emulator a
useMemory memory action = ask >>= liftIO . action . memory
{-# INLINE useMemory #-}

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

-- Sends an interrupt to the CPU
sendInterrupt :: Interrupt -> Emulator ()
sendInterrupt interrupt = useCpu (`modifyIORefU` (`setBit` fromEnum interrupt)) intr