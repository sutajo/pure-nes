{-# LANGUAGE DeriveAnyClass #-}

module Nes.PPU (
    PPU(..),
    Pixel,
    Palette(..),
    Sprite(..),
    powerUp,
    loadPalette,
    module Nes.CPU6502,
    module Data.Bits,
    module Data.IORef,
    module Data.IORef.Unboxed,
    module Data.Word
) where

import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import           Data.Store
import           GHC.Generics
import           Data.Bits
import           Data.Functor
import           Data.Word
import           Data.IORef
import           Data.IORef.Unboxed
import           Nes.CPU6502 (Register8, Register16)
import           Nes.Cartridge

type Pixel = (Word8, Word8, Word8)
newtype Palette = Palette (VU.Vector Pixel) deriving (Show, Generic, Store)

loadPalette :: FilePath -> IO B.ByteString
loadPalette path = do 
    let 
      parseByte = A.decimal <* A.skipSpace 
      parser    = A.skipSpace *> A.many' parseByte <* A.endOfInput 
    result <- B.readFile path <&> A.parseOnly parser
    case result of
        Left error -> fail $ "Failed to load palette data. Reason: " ++ show error
        Right bytelist -> pure (B.pack bytelist)

data Sprite = Sprite {
    cycleTimer :: !Int,
    pattLsb    :: !Word8,
    pattMsb    :: !Word8,
    paletteId  :: !Word8,
    behindBgd  :: !Bool,
    flipHori   :: !Bool,
    spriteZero :: !Bool
} deriving (Show, Generic, Store)

data PPU = PPU {
    palette         :: Palette,
    screen          :: VSM.IOVector Word8,
    nametable       :: VUM.IOVector Word8,
    paletteIndices  :: VUM.IOVector Word8,
    primaryOam      :: VUM.IOVector Word8,
    secondaryOam    :: IORef [Sprite],

    -- Registers visible to the CPU
    ppuCtrl           :: Register8,
    ppuMask           :: Register8,
    ppuStatus         :: Register8,
    ppuOamAddr        :: Register8,
    ppuOamData        :: Register8,
    ppuScroll         :: Register8,
    ppuAddr           :: Register8,
    ppuData           :: Register8,

    -- Internal registers used exclusively by the PPU
    pvtDataBuffer     :: Register8,
    pvtVRamAddr       :: Register16,
    pvtTempAddr       :: Register16,
    pvtAddressLatch   :: Register8,
    pvtFineX          :: Register8,

    -- Registers used for emulation
    emuCycle          :: IORefU Int,
    emuScanLine       :: IORefU Int,
    emuFrameCount     :: IORefU Word,
    emuClocks         :: IORefU Word,
    emuLastStatusRead :: IORefU Word,
    emuNmiPending     :: Register8,
    emuNmiOccured     :: Register8,
    emuNextNT         :: Register8,
    emuNextAT         :: Register8,
    emuNextLSB        :: Register8,
    emuNextMSB        :: Register8,
    emuPattShifterLo  :: Register16,
    emuPattShifterHi  :: Register16,
    emuAttrShifterLo  :: Register16,
    emuAttrShifterHi  :: Register16,
    -- Mirroring function
    mirrorNametableAddress :: Word16 -> Word16
}

--http://wiki.nesdev.com/w/index.php/Mirroring#Nametable_Mirroring

{-
2000 -> 2000
2400 -> 2000
2800 -> 2400
2C00 -> 2400
-}

horizontalMirroring :: Word16 -> Word16
horizontalMirroring addr = a - ((a .&. 0x800) `shiftR` 1)
  where a = (addr .&. 0xFFF) .&. complement 0x400

verticalMirroring :: Word16 -> Word16
verticalMirroring = (.&. 0x7FF)

powerUp :: Mirroring -> IO PPU
powerUp mirroring = 
    PPU                             <$>
    pure palette2C02                <*>
    VSM.replicate (256*240*3) 0     <*>
    VUM.new (2 * 0x400)             <*>
    VUM.new 0x20                    <*>
    VUM.new 256                     <*>
    newIORef []                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    newIORefU 0                     <*>
    pure (
      case mirroring of
        Horizontal -> horizontalMirroring
        Vertical   -> verticalMirroring
        __________ -> error $ "PPU emulator does not support this mirroring type: " ++ show mirroring
      )

palette2C02 = 
  Palette . VU.fromList $
    [
     (84, 84, 84 )
    ,(0,  30, 116)
    ,(8,  16, 144)
    ,(48, 0,  136)
    ,(68, 0,  100)
    ,(92, 0,  48 )
    ,(84, 4,  0  ) 
    ,(60, 24, 0  )
    ,(32, 42, 0  )
    ,(8,  58, 0  )
    ,(0,  64, 0  )
    ,(0,  60, 0  )
    ,(0,  50, 60 )
    ,(0,  0,  0  )
    ,(0,  0,  0  )
    ,(0,  0,  0  )
    ,(152,150,152)
    ,(8,  76, 196)
    ,(48, 50, 236)
    ,(92, 30, 228)
    ,(136,20, 176)
    ,(160,20, 100)
    ,(152,34, 32 )
    ,(120,60, 0  )
    ,(84, 90, 0  )
    ,(40, 114,0  )
    ,(8,  124,0  )
    ,(0,  118,40 )
    ,(0,  102,120)
    ,(0,  0,  0  )
    ,(0,  0,  0  )
    ,(0,  0,  0  )
    ,(236,238,236)
    ,(76, 154,236)
    ,(120,124,236)
    ,(176,98, 236)
    ,(228,84, 236)
    ,(236,88, 180)
    ,(236,106,100)
    ,(212,136,32 )
    ,(160,170,0  )
    ,(116,196,0  )
    ,(76, 208,32 )
    ,(56, 204,108)
    ,(56, 180,204)
    ,(60, 60, 60 )
    ,(0,  0,  0  )
    ,(0,  0,  0  )
    ,(236,238,236)
    ,(168,204,236)
    ,(188,188,236)
    ,(212,178,236)
    ,(236,174,236)
    ,(236,174,212)
    ,(236,180,176)
    ,(228,196,144)
    ,(204,210,120)
    ,(180,222,120)
    ,(168,226,144)
    ,(152,226,180)
    ,(160,214,228)
    ,(160,162,160)
    ,(0,  0,  0  )
    ,(0,  0,  0  ) 
    ]


data PpuSnapshot = PpuSnapshot {
    palette'           :: Palette,
    --screen            :: VSM.IOVector Word8,
    nametable'         :: VU.Vector Word8,
    paletteIndices'    :: VU.Vector Word8,
    primaryOam'        :: VU.Vector Word8,
    secondaryOam'      :: [Sprite],

    -- Registers visible to the CPU
    ppuCtrl'           :: Word8,
    ppuMask'           :: Word8,
    ppuStatus'         :: Word8,
    ppuOamAddr'        :: Word8,
    ppuOamData'        :: Word8,
    ppuScroll'         :: Word8,
    ppuAddr'           :: Word8,
    ppuData'           :: Word8,

    -- Internal registers used exclusively by the PPU
    pvtDataBuffer'     :: Word8,
    pvtVRamAddr'       :: Word16,
    pvtTempAddr'       :: Word16,
    pvtAddressLatch'   :: Word8,
    pvtFineX'          :: Word8,

    -- Registers used for emulation
    emuCycle'          :: Int,
    emuScanLine'       :: Int,
    emuFrameCount'     :: Word,
    emuClocks'         :: Word,
    emuLastStatusRead' :: Word,
    emuNmiPending'     :: Word8,
    emuNmiOccured'     :: Word8,
    emuNextNT'         :: Word8,
    emuNextAT'         :: Word8,
    emuNextLSB'        :: Word8,
    emuNextMSB'        :: Word8,
    emuPattShifterLo'  :: Word16,
    emuPattShifterHi'  :: Word16,
    emuAttrShifterLo'  :: Word16,
    emuAttrShifterHi'  :: Word16,
    -- Mirroring function
    mirroringType :: Mirroring
} deriving (Generic, Store)

getPpuSnapshot :: Mirroring -> PPU -> IO PpuSnapshot
getPpuSnapshot mirroringType PPU{..} = do
  let palette' = palette
  nametable'         <- VU.freeze nametable
  paletteIndices'    <- VU.freeze paletteIndices
  primaryOam'        <- VU.freeze primaryOam
  secondaryOam'      <- readIORef secondaryOam
  ppuCtrl'           <- readIORefU ppuCtrl
  ppuData'           <- readIORefU ppuData
  ppuMask'           <- readIORefU ppuMask
  ppuStatus'         <- readIORefU ppuStatus
  ppuOamAddr'        <- readIORefU ppuOamAddr
  ppuOamData'        <- readIORefU ppuOamData
  ppuScroll'         <- readIORefU ppuScroll
  ppuAddr'           <- readIORefU ppuAddr
  pvtDataBuffer'     <- readIORefU pvtDataBuffer
  pvtAddressLatch'   <- readIORefU pvtAddressLatch
  pvtFineX'          <- readIORefU pvtFineX
  pvtVRamAddr'       <- readIORefU pvtVRamAddr
  pvtTempAddr'       <- readIORefU pvtTempAddr
  emuFrameCount'     <- readIORefU emuFrameCount
  emuCycle'          <- readIORefU emuCycle
  emuScanLine'       <- readIORefU emuScanLine
  emuClocks'         <- readIORefU emuClocks
  emuLastStatusRead' <- readIORefU emuLastStatusRead
  emuNmiPending'     <- readIORefU emuNmiPending
  emuNmiOccured'     <- readIORefU emuNmiOccured
  emuNextNT'         <- readIORefU emuNextNT
  emuNextAT'         <- readIORefU emuNextAT
  emuNextLSB'        <- readIORefU emuNextLSB
  emuNextMSB'        <- readIORefU emuNextMSB
  emuPattShifterLo'  <- readIORefU emuPattShifterLo
  emuPattShifterHi'  <- readIORefU emuPattShifterHi
  emuAttrShifterLo'  <- readIORefU emuAttrShifterLo
  emuAttrShifterHi'  <- readIORefU emuAttrShifterHi
  return PpuSnapshot{..}

thawPpuSnapshot :: PpuSnapshot -> IO PPU
thawPpuSnapshot PpuSnapshot{..} = do
  let 
    palette = palette'
    mirrorNametableAddress = case mirroringType of
      Horizontal -> horizontalMirroring
      Vertical   -> verticalMirroring
      __________ -> error $ "PPU emulator does not support this mirroring type: " ++ show mirroringType
  screen             <- VSM.replicate (256*240*3) 0
  nametable          <- VU.thaw nametable'
  paletteIndices     <- VU.thaw paletteIndices'
  primaryOam         <- VU.thaw primaryOam'
  secondaryOam       <- newIORef secondaryOam'
  ppuCtrl            <- newIORefU ppuCtrl'
  ppuData            <- newIORefU ppuData'
  ppuMask            <- newIORefU ppuMask'
  ppuStatus          <- newIORefU ppuStatus'
  ppuOamAddr         <- newIORefU ppuOamAddr'
  ppuOamData         <- newIORefU ppuOamData'
  ppuScroll          <- newIORefU ppuScroll'
  ppuAddr            <- newIORefU ppuAddr'
  pvtDataBuffer      <- newIORefU pvtDataBuffer'
  pvtAddressLatch    <- newIORefU pvtAddressLatch'
  pvtFineX           <- newIORefU pvtFineX'
  pvtVRamAddr        <- newIORefU pvtVRamAddr'
  pvtTempAddr        <- newIORefU pvtTempAddr'
  emuFrameCount      <- newIORefU emuFrameCount'
  emuCycle           <- newIORefU emuCycle'
  emuScanLine        <- newIORefU emuScanLine'
  emuClocks          <- newIORefU emuClocks'
  emuLastStatusRead  <- newIORefU emuLastStatusRead'
  emuNmiPending      <- newIORefU emuNmiPending'
  emuNmiOccured      <- newIORefU emuNmiOccured'
  emuNextNT          <- newIORefU emuNextNT'
  emuNextAT          <- newIORefU emuNextAT'
  emuNextLSB         <- newIORefU emuNextLSB'
  emuNextMSB         <- newIORefU emuNextMSB'
  emuPattShifterLo   <- newIORefU emuPattShifterLo'
  emuPattShifterHi   <- newIORefU emuPattShifterHi'
  emuAttrShifterLo   <- newIORefU emuAttrShifterLo'
  emuAttrShifterHi   <- newIORefU emuAttrShifterHi'
  return PPU{..}