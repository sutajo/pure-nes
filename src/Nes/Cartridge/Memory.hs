{-# LANGUAGE DeriveAnyClass #-}

module Nes.Cartridge.Memory where


import           Data.ByteString
import           Data.Serialize (Serialize)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           GHC.Generics


data INES = INES {
    title         :: ByteString,
    flags6        :: Word8,
    flags7        :: Word8,
    flags9        :: Word8,
    chr_rom_bs    :: ByteString,
    prg_rom_bs    :: ByteString,
    prg_ram_size  :: Int 
} deriving (Show)


data Mirroring
  = Horizontal
  | Vertical
  | FourScreen
  deriving (Show, Enum, Generic, Serialize)


newtype MapperState = MapperState [Word8] 
  deriving (Generic, Serialize)


data Mapper = Mapper {
    cpuRead     :: Word16 -> IO Word8
 ,  cpuWrite    :: Word16 -> Word8 -> IO ()
 ,  ppuRead     :: Word16 -> IO Word8
 ,  ppuWrite    :: Word16 -> Word8 -> IO ()
 ,  serialize   :: IO MapperState
 ,  deserialize :: MapperState -> IO ()
} deriving (Generic)


data Cartridge = Cartridge {
    hasChrRam    :: Bool,
    mapperId     :: Word8,
    mapper       :: Mapper,
    mirror       :: Mirroring,
    chr_rom      :: VUM.IOVector Word8,
    prg_rom      :: VUM.IOVector Word8,
    prg_ram      :: VUM.IOVector Word8
} deriving (Generic)

cpuReadCartridge :: Cartridge -> Word16 -> IO Word8
cpuReadCartridge cart = cpuRead (mapper cart)

cpuWriteCartridge :: Cartridge -> Word16 -> Word8 -> IO ()
cpuWriteCartridge cart = cpuWrite (mapper cart)

ppuReadCartridge :: Cartridge -> Word16 -> IO Word8
ppuReadCartridge cart = ppuRead (mapper cart)

ppuWriteCartridge :: Cartridge -> Word16 -> Word8 -> IO ()
ppuWriteCartridge cart = ppuWrite (mapper cart)