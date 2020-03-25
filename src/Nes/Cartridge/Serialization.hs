{-# LANGUAGE DeriveAnyClass #-}

module Nes.Cartridge.Serialization (
  Cartridge,
  serialize,
  deserialize
) where


import qualified Data.Map                    as M
import qualified Data.Vector.Unboxed         as VU
import           Data.Word
import           GHC.Generics
import           Nes.Emulation.Monad
import           Nes.Cartridge.Parser hiding (Cartridge, serialize, deserialize)
import qualified Nes.Cartridge.Parser as P

data Cartridge = Cartridge {
  shasChrRam    :: Bool,
  smapperId     :: Word8,
  smapperState  :: MapperState,
  smirror       :: Mirroring,
  schr_rom      :: VU.Vector Word8,
  sprg_rom      :: VU.Vector Word8,
  sprg_ram      :: VU.Vector Word8
} deriving (Generic)

serialize :: Emulator Cartridge
serialize = undefined

deserialize :: Cartridge -> IO P.Cartridge
deserialize Cartridge{..} = undefined