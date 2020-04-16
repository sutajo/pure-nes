{-# LANGUAGE DeriveAnyClass #-}

module Nes.Cartridge.Serialization (
  Cartridge,
  serialize,
  deserialize
) where


import           Data.Serialize
import qualified Data.Map                    as M
import qualified Data.Vector.Unboxed         as VU
import           Data.Word
import           GHC.Generics
import           Nes.Emulation.Monad
import qualified Nes.Cartridge.Memory as CM
import           Nes.Cartridge.Mappers hiding (serialize, deserialize)


data Cartridge = Cartridge {
  shasChrRam    :: Bool,
  smapperId     :: Word8,
  smapperState  :: CM.MapperState,
  smirror       :: CM.Mirroring,
  schr_rom      :: VU.Vector Word8,
  sprg_rom      :: VU.Vector Word8,
  sprg_ram      :: VU.Vector Word8
} deriving (Generic, Serialize)


serialize :: Emulator CM.Cartridge Cartridge
serialize = do
  CM.Cartridge{..} <- ask
  let shasChrRam = hasChrRam
  let smapperId = mapperId
  let smirror   = mirror
  schr_rom <- liftIO $ VU.freeze chr_rom
  sprg_rom <- liftIO $ VU.freeze prg_rom
  sprg_ram <- liftIO $ VU.freeze prg_ram
  smapperState <- liftIO $ CM.serialize mapper
  return Cartridge{..}


deserialize :: Cartridge -> IO CM.Cartridge
deserialize Cartridge{..} = do
  let hasChrRam = shasChrRam
  let mapperId = smapperId
  let mirror   = smirror
  chr_rom <- VU.thaw schr_rom
  prg_rom <- VU.thaw sprg_rom
  prg_ram <- VU.thaw sprg_ram
  let mapper = CM.dummyMapper
  let cart   = CM.Cartridge{..}
  assembledMapper <- (mappersById M.! mapperId) cart
  CM.deserialize assembledMapper smapperState
  return cart { CM.mapper = assembledMapper }

