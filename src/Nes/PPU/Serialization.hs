{-# LANGUAGE DeriveAnyClass #-}

module Nes.PPU.Serialization (
    PPU,
    serialize,
    deserialize
) where

import           Data.Store
import           GHC.Generics
import           Data.IORef
import           Data.IORef.Unboxed
import           Data.Word
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Storable.Mutable as VSM
import           Nes.PPU.Memory hiding (PPU)
import qualified Nes.PPU.Memory as M
import           Nes.Cartridge.Parser

data PPU = PPU {
    spalette           :: M.Palette,
    snametable         :: VU.Vector Word8,
    spaletteIndices    :: VU.Vector Word8,
    sprimaryOam        :: VU.Vector Word8,
    ssecondaryOam      :: [M.Sprite],

    -- Registers visible to the CPU
    sppuCtrl           :: Word8,
    sppuMask           :: Word8,
    sppuStatus         :: Word8,
    sppuOamAddr        :: Word8,
    sppuOamData        :: Word8,
    sppuScroll         :: Word8,
    sppuAddr           :: Word8,
    sppuData           :: Word8,

    -- Internal registers used exclusively by the PPU
    spvtDataBuffer     :: Word8,
    spvtVRamAddr       :: Word16,
    spvtTempAddr       :: Word16,
    spvtAddressLatch   :: Word8,
    spvtFineX          :: Word8,

    -- Registers used for emulation
    semuCycle          :: Int,
    semuScanLine       :: Int,
    semuFrameCount     :: Word,
    semuClocks         :: Word,
    semuLastStatusRead :: Word,
    semuNmiPending     :: Word8,
    semuNmiOccured     :: Word8,
    semuNextNT         :: Word8,
    semuNextAT         :: Word8,
    semuNextLSB        :: Word8,
    semuNextMSB        :: Word8,
    semuPattShifterLo  :: Word16,
    semuPattShifterHi  :: Word16,
    semuAttrShifterLo  :: Word16,
    semuAttrShifterHi  :: Word16,
    -- Mirroring function
    smirroringType :: Mirroring
} deriving (Generic, Store)

serialize :: Mirroring -> M.PPU -> IO PPU
serialize smirroringType M.PPU{..} = do
  let spalette = palette
  snametable         <- VU.freeze nametable
  spaletteIndices    <- VU.freeze paletteIndices
  sprimaryOam        <- VU.freeze primaryOam
  ssecondaryOam      <- readIORef secondaryOam
  sppuCtrl           <- readIORefU ppuCtrl
  sppuData           <- readIORefU ppuData
  sppuMask           <- readIORefU ppuMask
  sppuStatus         <- readIORefU ppuStatus
  sppuOamAddr        <- readIORefU ppuOamAddr
  sppuOamData        <- readIORefU ppuOamData
  sppuScroll         <- readIORefU ppuScroll
  sppuAddr           <- readIORefU ppuAddr
  spvtDataBuffer     <- readIORefU pvtDataBuffer
  spvtAddressLatch   <- readIORefU pvtAddressLatch
  spvtFineX          <- readIORefU pvtFineX
  spvtVRamAddr       <- readIORefU pvtVRamAddr
  spvtTempAddr       <- readIORefU pvtTempAddr
  semuFrameCount     <- readIORefU emuFrameCount
  semuCycle          <- readIORefU emuCycle
  semuScanLine       <- readIORefU emuScanLine
  semuClocks         <- readIORefU emuClocks
  semuLastStatusRead <- readIORefU emuLastStatusRead
  semuNmiPending     <- readIORefU emuNmiPending
  semuNmiOccured     <- readIORefU emuNmiOccured
  semuNextNT         <- readIORefU emuNextNT
  semuNextAT         <- readIORefU emuNextAT
  semuNextLSB        <- readIORefU emuNextLSB
  semuNextMSB        <- readIORefU emuNextMSB
  semuPattShifterLo  <- readIORefU emuPattShifterLo
  semuPattShifterHi  <- readIORefU emuPattShifterHi
  semuAttrShifterLo  <- readIORefU emuAttrShifterLo
  semuAttrShifterHi  <- readIORefU emuAttrShifterHi
  return PPU{..}


deserialize :: PPU -> IO M.PPU
deserialize PPU{..} = do
  let 
    palette = spalette
    mirrorNametableAddress = case smirroringType of
      Horizontal -> horizontalMirroring
      Vertical   -> verticalMirroring
      __________ -> error $ "PPU emulator does not support this mirroring type: " ++ show smirroringType
  screen             <- VSM.replicate (256*240*3) 0
  nametable          <- VU.thaw snametable
  paletteIndices     <- VU.thaw spaletteIndices
  primaryOam         <- VU.thaw sprimaryOam
  secondaryOam       <- newIORef ssecondaryOam
  ppuCtrl            <- newIORefU sppuCtrl
  ppuData            <- newIORefU sppuData
  ppuMask            <- newIORefU sppuMask
  ppuStatus          <- newIORefU sppuStatus
  ppuOamAddr         <- newIORefU sppuOamAddr
  ppuOamData         <- newIORefU sppuOamData
  ppuScroll          <- newIORefU sppuScroll
  ppuAddr            <- newIORefU sppuAddr
  pvtDataBuffer      <- newIORefU spvtDataBuffer
  pvtAddressLatch    <- newIORefU spvtAddressLatch
  pvtFineX           <- newIORefU spvtFineX
  pvtVRamAddr        <- newIORefU spvtVRamAddr
  pvtTempAddr        <- newIORefU spvtTempAddr
  emuFrameCount      <- newIORefU semuFrameCount
  emuCycle           <- newIORefU semuCycle
  emuScanLine        <- newIORefU semuScanLine
  emuClocks          <- newIORefU semuClocks
  emuLastStatusRead  <- newIORefU semuLastStatusRead
  emuNmiPending      <- newIORefU semuNmiPending
  emuNmiOccured      <- newIORefU semuNmiOccured
  emuNextNT          <- newIORefU semuNextNT
  emuNextAT          <- newIORefU semuNextAT
  emuNextLSB         <- newIORefU semuNextLSB
  emuNextMSB         <- newIORefU semuNextMSB
  emuPattShifterLo   <- newIORefU semuPattShifterLo
  emuPattShifterHi   <- newIORefU semuPattShifterHi
  emuAttrShifterLo   <- newIORefU semuAttrShifterLo
  emuAttrShifterHi   <- newIORefU semuAttrShifterHi
  return M.PPU{..}