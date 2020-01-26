{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nes.EmulatorMonad where

import Control.Monad.Reader
import Data.Array.IO (IOUArray)
import Data.Functor ((<&>))
import Data.Word (Word8, Word16)
import Nes.CPU6502 (CPU)
import Nes.PPU (PPU)

type RAM = IOUArray Word16 Word8

data Nes =  Nes {
    cpu         ::  CPU,
    ppu         ::  PPU,
    ram         ::  RAM,
    ioRegisters ::  IOUArray Word16 Word8 
}

newtype Emulator a = Emulator { 
    unEmulator :: ReaderT Nes IO a 
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader Nes)


runEmulator :: Emulator a -> Nes -> IO a
runEmulator (Emulator emu) = runReaderT emu

useMemory :: (Nes -> b) -> (b-> IO a) -> Emulator a
useMemory memory action = ask <&> memory >>= liftIO . action 