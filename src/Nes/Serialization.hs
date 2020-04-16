{-# LANGUAGE DeriveAnyClass #-}

module Nes.Serialization (
    Nes,
    serialize,
    deserialize
) where

import           Data.Serialize
import           GHC.Generics
import qualified Nes.APU.Memory               as APU
import qualified Nes.CPU.Serialization        as CPU
import qualified Nes.PPU.Serialization        as PPU
import           Nes.Cartridge.Memory (getCPUAccess, getPPUAccess)
import qualified Nes.Cartridge.Serialization  as Cartridge
import           Nes.Emulation.Monad hiding (CPU, APU, Nes, ram, controllers)
import qualified Nes.Emulation.Monad as M

data Nes = Nes {
    cpu         :: CPU.CPU,
    ppu         :: PPU.PPU,
    apu         :: APU.APU,
    cartridge   :: Cartridge.Cartridge
} deriving (Generic, Serialize)

serialize :: Emulator M.Nes Nes
serialize = do
  Nes                                       <$>
    M.emulateSubcomponent M.cpu CPU.serialize   <*>
    M.emulateSubcomponent M.ppu PPU.serialize   <*>
    getApu                                  <*>
    M.emulateSubcomponent M.cartridge Cartridge.serialize

deserialize :: Nes -> IO M.Nes
deserialize Nes{..} = do
  let 
    CPU.CPU{..} = cpu
  interrupts <- CPU.deserializeInterruptAccess interruptAccess
  cartridge  <- Cartridge.deserialize cartridge
  ppu        <- PPU.deserialize (getPPUAccess cartridge) interrupts ppu 
  cpu        <- CPU.deserialize (getCPUAccess cartridge) (PPUAccess ppu) interrupts cpu
  apu        <- newIORef apu
  return $ M.Nes cpu ppu apu cartridge
    

