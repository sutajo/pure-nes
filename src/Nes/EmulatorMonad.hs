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
    sendPendingNmi,
    sendPendingIrq,
    sendNmi,
    clearNmi,
    sendIrq,
    useCpu,
    readController,
    writeController,
    processInput
) where

import           Control.Monad.Reader
import           Data.Array.IO
import           Data.Word (Word8, Word16)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM
import           Data.IORef.Unboxed
import           Data.Functor
import           Nes.CPU6502   as CPU
import           Nes.PPU       as PPU
import qualified Nes.Cartridge as Cart
import           Nes.APU       as APU
import qualified Nes.Controls  as Controls

type RAM = IOUArray Word16 Word8

allocateRAM :: IO RAM
allocateRAM = newArray (0, 0x07FF) 0

data Nes =  Nes {
    cpu         ::  CPU,
    ram         ::  RAM,
    ppu         ::  PPU,
    apu         ::  APU,
    cartridge   ::  Cart.Cartridge,
    controllers ::  IOVector Controls.Controller
}

powerUpNes :: Cart.Cartridge -> IO Nes
powerUpNes cart = 
    Nes         <$>
    CPU.powerUp <*>
    allocateRAM <*>
    PPU.powerUp (Cart.mirror cart) <*>
    APU.powerUp <*>
    pure cart   <*>
    VM.replicate 2 Controls.powerUp

newtype Emulator a = Emulator (ReaderT Nes IO a) 
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Nes, MonadFail)


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

-- Sends an NMI which fires after the specified amount of clock cycles
sendPendingInterrupt :: (CPU -> Register) -> Word8 -> Emulator ()
sendPendingInterrupt reg cyc = useCpu (`writeIORefU` cyc) reg

clearInterrupt :: (CPU -> Register) -> Emulator ()
clearInterrupt reg = useCpu (`writeIORefU` 0) reg

sendPendingNmi = sendPendingInterrupt nmiTimer
sendNmi  = sendPendingNmi 1
clearNmi = clearInterrupt nmiTimer
sendPendingIrq = sendPendingInterrupt irqTimer
sendIrq = sendPendingIrq 1

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
    