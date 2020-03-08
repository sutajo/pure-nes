{-# LANGUAGE DeriveAnyClass #-}

module Nes.Serialization (
    Nes,
    serialize,
    deserialize
) where

import           Data.Store
import           Data.Word
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxed          as VU
import           GHC.Generics
import qualified Nes.APU.Memory               as APU
import qualified Nes.CPU.Serialization        as CPU
import qualified Nes.PPU.Serialization        as PPU
import qualified Nes.Cartridge.Serialization  as Cartridge
import qualified Nes.Controls                 as Controls
import           Nes.Emulation.Monad hiding (CPU, APU, Nes, ram, controllers)
import qualified Nes.Emulation.Monad as M

data Nes = Nes {
    cpu         :: CPU.CPU,
    ram         :: VU.Vector Word8,
    ppu         :: PPU.PPU,
    apu         :: APU.APU,
    cartridge   :: Cartridge.Cartridge,
    controllers :: V.Vector (Controls.Controller)
} deriving (Generic, Store)

serialize :: Emulator Nes
serialize = do
  ram         <- ask <&> M.ram
  controllers <- ask <&> M.controllers
  Nes                      <$>
    CPU.serialize          <*>
    liftIO (VU.freeze ram) <*>
    PPU.serialize          <*>
    getApu                 <*>
    Cartridge.serialize    <*>
    liftIO (V.freeze controllers)

deserialize :: Nes -> IO M.Nes
deserialize Nes{..} = do
  M.Nes                             <$>
    CPU.deserialize cpu             <*>
    VU.thaw ram                     <*>
    PPU.deserialize ppu             <*>
    newIORef apu                    <*>
    Cartridge.deserialize cartridge <*>
    VM.replicate 2 (Controls.powerUp)



