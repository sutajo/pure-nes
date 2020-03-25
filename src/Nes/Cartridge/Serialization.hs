{-# LANGUAGE DeriveAnyClass #-}

module Nes.Cartridge.Serialization (
  Cartridge,
  serialize,
  deserialize
) where


import           Data.Serialize
import qualified Data.Map                    as M
import qualified Data.Vector.Unboxed         as VU
import           Data.Vector.Serialize ()
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
} deriving (Generic, Serialize)

serialize :: Emulator Cartridge
serialize = do
  P.Cartridge{..} <- ask <&> cartridge
  let shasChrRam = hasChrRam
  let smapperId = mapperId
  let smirror   = mirror
  schr_rom <- liftIO $ VU.freeze chr_rom
  sprg_rom <- liftIO $ VU.freeze prg_rom
  sprg_ram <- liftIO $ VU.freeze prg_ram
  smapperState <- liftIO $ P.serialize mapper
  return Cartridge{..}

deserialize :: Cartridge -> IO P.Cartridge
deserialize Cartridge{..} = do
  let hasChrRam = shasChrRam
  let mapperId = smapperId
  let mirror   = smirror
  chr_rom <- VU.thaw schr_rom
  prg_rom <- VU.thaw sprg_rom
  prg_ram <- VU.thaw sprg_ram
  let mapper = dummyMapper
  let cart   = P.Cartridge{..}
  assembledMapper <- (mappersById M.! mapperId) cart
  P.deserialize assembledMapper smapperState
  return cart { mapper = assembledMapper } 