{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.EmulatorMonad where

import           Control.Monad.Reader
import           Data.Array.IO
import           Data.Functor ((<&>))
import           Data.Word (Word8, Word16)
import           Nes.CPU6502   as CPU
import           Nes.PPU       as PPU
import qualified Nes.Cartridge as Cart
import           Nes.APU       as APU

type RAM = IOUArray Word16 Word8

allocateRAM :: IO RAM
allocateRAM = newArray (0, 0x1FFF) 0

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
    PPU.powerUp <*>
    APU.powerUp <*>
    pure cart


newtype Emulator a = Emulator { 
    unEmulator :: ReaderT Nes IO a 
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Nes)


runEmulator :: Nes -> Emulator a -> IO a
runEmulator nes (Emulator emu) = runReaderT emu nes

useMemory :: (Nes -> b) -> (b -> IO a) -> Emulator a
useMemory memory action = ask <&> memory >>= liftIO . action

readCartridge :: Word16 -> Emulator Word8
readCartridge addr = useMemory cartridge $ \cart -> Cart.readCartridge cart addr

writeCartridge :: Word16 -> Word8 -> Emulator ()
writeCartridge addr val = useMemory cartridge $ \cart -> Cart.writeCartridge cart addr val