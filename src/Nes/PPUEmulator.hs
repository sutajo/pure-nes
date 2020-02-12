{-# LANGUAGE LambdaCase #-}

module Nes.PPUEmulator (
  clock,
  accessScreen,
  cpuReadRegister,
  cpuWriteRegister
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Primitive(Prim)
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.IORef.Unboxed
import           Data.Functor
import           Data.Word
import           Data.Bits
import           Prelude hiding (read)
import           SDL.Raw.Timer
import           Nes.PPU
import           Nes.EmulatorMonad

accessMemory :: (PPU -> a) -> Emulator a
accessMemory memory = ask <&> (memory . ppu)

accessScreen :: Emulator (VSM.IOVector Word8)
accessScreen = accessMemory screen

readPalette :: Word8 -> Emulator Pixel
readPalette index = do
  Palette p <- accessMemory palette
  pure $ p VU.! (fromIntegral index)

usePPU :: (b -> IO a) -> (PPU -> b) -> Emulator a
usePPU action field = useMemory (field . ppu) action

readPPUComponent :: Enum addr => (PPU -> VUM.IOVector Word8) -> addr -> Emulator Word8
readPPUComponent component addr = usePPU (`VUM.read` (fromEnum addr)) component

writePPUComponent :: Enum addr => (PPU -> VUM.IOVector Word8) -> addr -> Word8 -> Emulator ()
writePPUComponent component addr val = usePPU (\ind -> VUM.write ind (fromEnum addr) val) component

--http://wiki.nesdev.com/w/index.php/PPU_palettes
--Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C

usePaletteIndices usage addr
 | 0x10 <= addr && addr <= 0x1C && addr `rem` 4 == 0 = usage (addr - 0x10)
 | otherwise = usage addr
  
readPaletteIndices  = usePaletteIndices (readPPUComponent  paletteIndices)
writePaletteIndices = usePaletteIndices (writePPUComponent paletteIndices)

readNametable       = readPPUComponent  nametable
writeNametable      = writePPUComponent nametable

readReg :: Prim a => (PPU -> IORefU a) -> Emulator a
readReg = usePPU readIORefU

writeReg :: Prim a => (PPU -> IORefU a) -> a -> Emulator ()
writeReg reg val = usePPU (`writeIORefU` val) reg

modifyReg :: Prim a => (PPU -> IORefU a) -> (a -> a) -> Emulator ()
modifyReg reg f = readReg reg >>= writeReg reg . f

advanceVRAMAddress = do
  ppuCtrl <- readReg ppuCtrl
  modifyReg emuVRAMAddr (+ if ppuCtrl `testBit` 2 then 32 else 1)

--http://wiki.nesdev.com/w/index.php/PPU_registers
cpuReadRegister :: Word16 -> Emulator Word8
cpuReadRegister = 
  let
    readStatusReg = do
      result <- liftA2 (.|.) (readReg ppuStatus <&> (.&. 0xE0)) (readReg emuDataBuffer <&> (.&. 0x1F))
      modifyReg ppuStatus (`clearBit` 7) -- Clear vertical blank flag
      writeReg emuAddressLatch 0
      return (result `setBit` 7)
    readDataReg = do
      bufferOld <- readReg emuDataBuffer
      vramAddr  <- readReg emuVRAMAddr
      read vramAddr >>= writeReg emuDataBuffer
      bufferNew <- readReg emuDataBuffer
      let result = if vramAddr >= 0x3F00 then bufferNew else bufferOld  -- Palette RAM is not buffered
      advanceVRAMAddress    
      return result
  in
  \case
  0x2002 -> readStatusReg
  0x2007 -> readDataReg
  ______ -> pure 0

cpuWriteRegister :: Word16 -> Word8 -> Emulator ()
cpuWriteRegister addr val = case addr of
  0x2000 -> writeReg ppuCtrl val
  0x2001 -> writeReg ppuMask val
  0x2005 -> modifyReg emuAddressLatch complement
  0x2006 -> do
    let val16 = fromIntegral val
    latch <- readReg emuAddressLatch
    if latch == 0
    then do  -- higher byte is being written
      modifyReg emuVRAMAddr (\vramAddr -> ((val16 `shiftL` 8) .&. 0x3F00) .|. (vramAddr .&. 0xFF))
      writeReg emuAddressLatch 1
    else do  -- lower  byte is being written
      modifyReg emuVRAMAddr (\vramAddr -> (vramAddr .&. 0xFF00) .|. val16)
      writeReg emuAddressLatch 0
  0x2007 -> do
    emuVRAMAddr <- readReg emuVRAMAddr
    write emuVRAMAddr val
    advanceVRAMAddress
  ______ -> pure ()

read :: Word16 -> Emulator Word8
read addr
  | addr <= 0x1FFF = ppuReadCartridge addr
  | addr <= 0x3EFF = do
    mirror <- getNametableMirroring
    readNametable $ mirror (addr - 0x2000)
  | addr <= 0x3FFF = readPaletteIndices ((addr - 0x3F00) `rem` 0x20)

write :: Word16 -> Word8 -> Emulator ()
write addr val
  | addr <= 0x1FFF = ppuWriteCartridge addr val
  | addr <= 0x3EFF = do
    mirror <- getNametableMirroring
    writeNametable (mirror $ addr - 0x2000) val
  | addr <= 0x3FFF = writePaletteIndices ((addr - 0x3F00) `rem` 0x20) val

getColor :: Word8 -> Word8 -> Emulator Pixel
getColor palette pixel = do
  index <- read (0x3F00 + fromIntegral (palette `shiftL` 2 + pixel))
  readPalette index

{-
clock :: Emulator ()
clock = do
  _      <- read 0x1FFF
  screen <- accessScreen
  sec    <- getTicks
  let 
    r i = round (((sin $ fromIntegral sec / 1000 * 2 * pi / 10) + 1) / 2 * fromIntegral ((100 + fromIntegral i) `rem` 255))
    g i = round (((sin $ fromIntegral sec / 1000 * 2 * pi / 5) + 1) / 2 * fromIntegral ((100 - i) `mod` 255))
    b = 40
  forM_ [0,3..256*3*240-1] $ \i -> liftIO $ do
    VSM.write screen i (r i)
    VSM.write screen (i+1) (g i)
    VSM.write screen (i+2) b
-}

setPixel :: Word8 -> Word8 -> Pixel -> Emulator ()
setPixel x y (r,g,b) = do
  screen <- accessScreen
  let offset = 256*3*fromIntegral y + fromIntegral x*3
  liftIO $ do
    VSM.write screen  offset      r
    VSM.write screen (offset + 1) g
    VSM.write screen (offset + 2) b

clock :: Emulator ()
clock = do
  screen <- accessScreen
  liftIO $ VSM.set screen 0
  forM_ [0..1] $ \patterntab ->
    forM_ [0..15] $ \y -> do
      let rowOffset = y * 256
      forM_ [0..15] $ \x -> do
        let offset = rowOffset + x*16
        forM_ [0..7] $ \row -> do
          tile_lsb <- read (patterntab * 0x1000 + offset + row)
          tile_msb <- read (patterntab * 0x1000 + offset + row + 8)
          forM_ [0..7] $ \col -> do
            let 
              c = fromIntegral col
              get a i = fromEnum $ a `testBit` i 
            let pixel = fromIntegral $ ((tile_msb `get` c) `shiftL` 1) .|. (tile_lsb `get` c)
            color <- getColor 0 pixel
            setPixel (fromIntegral patterntab * 128 + fromIntegral(x*8 + (7 - col))) (fromIntegral(y*8 + row)) color
  forM_ [0..7] $ \palette ->
    forM_ [0..3] $ \pixel ->
      forM_ [0..4] $ \i -> do
        setPixel (palette * 25 + pixel*5 + i) 179 (255, 255, 255)
        setPixel (palette * 25 + pixel*5 + i) 185 (255, 255, 255)
        forM_ [0..4] $ \j -> do
          color <- getColor palette pixel
          setPixel (palette * 25 + pixel*5 + i) (180 + j) color
