{-# LANGUAGE DeriveAnyClass, StrictData #-}

module Nes.Cartridge.Memory where


import           Data.Bits
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
 ,  mirroringFunction :: IO (Word16 -> Word16)
} deriving (Generic)


data CartridgeAccess = CartridgeAccess {
    readCartridge    :: Word16 -> IO Word8
  , writeCartridge   :: Word16 -> Word8 -> IO ()
  , getMirroring     :: IO (Word16 -> Word16)
}


data Cartridge = Cartridge {
    hasChrRam    :: Bool,
    mapperId     :: Word8,
    mapper       :: Mapper,
    mirror       :: Mirroring,
    chr          :: VUM.IOVector Word8,
    prg_rom      :: VUM.IOVector Word8,
    prg_ram      :: VUM.IOVector Word8
} deriving (Generic)


dummyMapper = 
  Mapper 
  dummyRead 
  dummyWrite 
  dummyRead 
  dummyWrite 
  (pure $ MapperState []) 
  (\_ -> undefined)
  (pure (\_ -> undefined))
 where dummyRead = const (pure 0); dummyWrite _ _ = pure ()


getCPUAccess :: Cartridge -> CartridgeAccess
getCPUAccess Cartridge{mapper=Mapper{cpuRead, cpuWrite}} = 
  CartridgeAccess 
  cpuRead 
  cpuWrite 
  (pure $ \_ -> error "Querying mirroring from the CPU is not allowed.")

getPPUAccess :: Cartridge -> CartridgeAccess
getPPUAccess Cartridge{mapper=Mapper{ppuRead, ppuWrite, mirroringFunction}} = CartridgeAccess ppuRead ppuWrite mirroringFunction


horizontalMirroring :: Word16 -> Word16
horizontalMirroring addr = a - ((a .&. 0x800) `shiftR` 1)
  where a = (addr .&. 0xFFF) .&. complement 0x400


verticalMirroring :: Word16 -> Word16
verticalMirroring = (.&. 0x7FF)


getMirroringFunction :: Mirroring -> (Word16 -> Word16)
getMirroringFunction mirroring = case mirroring of
  Horizontal -> horizontalMirroring
  Vertical   -> verticalMirroring
  __________ -> error $ "PPU emulator does not support this mirroring type: " ++ show mirroring