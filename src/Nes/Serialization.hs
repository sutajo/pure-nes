{-# LANGUAGE DeriveAnyClass #-}

module Nes.Serialization where

import           Data.Store
import           Data.Word
import qualified Data.Vector.Unboxed          as VU
import           GHC.Generics
import           Nes.APU.Memory               as APU
import           Nes.CPU.Serialization        as CPU
import           Nes.PPU.Serialization        as PPU
import           Nes.Cartridge.Serialization
import qualified Nes.Controls                 as Controls

data Nes = Nes {
    cpu         :: CPU,
    ram         :: VU.Vector Word8,
    ppu         :: PPU,
    apu         :: APU,
    cartridge   :: Cartridge,
    controllers :: [Controls.Controller]
} deriving (Generic, Store)