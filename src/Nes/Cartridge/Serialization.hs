{-# LANGUAGE DeriveAnyClass #-}

module Nes.Cartridge.Serialization (
  Cartridge,
  serialize,
  deserialize
) where


import           Data.Store
import qualified Data.Map                    as M
import qualified Data.Vector.Unboxed         as VU
import           Data.Word
import           GHC.Generics
import           Nes.Emulation.Monad
import           Nes.Cartridge.Parser hiding (Cartridge)
import qualified Nes.Cartridge.Parser as P

data Cartridge = Cartridge {
  shasChrRam    :: Bool,
  smapperId     :: Word8,
  smirror       :: Mirroring,
  schr_rom      :: VU.Vector Word8,
  sprg_rom      :: VU.Vector Word8,
  sprg_ram      :: VU.Vector Word8
} deriving (Generic, Store)

serialize :: P.Cartridge -> IO Cartridge
serialize P.Cartridge{..} = do
  let shasChrRam = hasChrRam
  let smapperId = mapperId
  let smirror   = mirror
  schr_rom <- VU.freeze chr_rom
  sprg_rom <- VU.freeze prg_rom
  sprg_ram <- VU.freeze prg_ram
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
  return cart { mapper = assembledMapper } 