{-# LANGUAGE LambdaCase #-}

module Nes.PPUEmulator (
  clock,
  accessScreen,
  cpuReadRegister,
  cpuWriteRegister,
  drawPatternTable,
  drawPalette
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
import           Nes.CPU6502
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
  modifyReg pvtVRAMAddr (+ if ppuCtrl `testBit` 2 then 32 else 1)

--http://wiki.nesdev.com/w/index.php/PPU_registers
cpuReadRegister :: Word16 -> Emulator Word8
cpuReadRegister = 
  let
    readStatusReg = do
      result <- liftA2 (.|.) (readReg ppuStatus <&> (.&. 0xE0)) (readReg pvtDataBuffer <&> (.&. 0x1F))
      modifyReg ppuStatus (`clearBit` 7) -- Clear vertical blank flag
      writeReg pvtAddressLatch 0
      return $ result
    readDataReg = do
      bufferOld <- readReg pvtDataBuffer
      vramAddr  <- readReg pvtVRAMAddr
      read vramAddr >>= writeReg pvtDataBuffer
      bufferNew <- readReg pvtDataBuffer
      let result = if vramAddr >= 0x3F00 then bufferNew else bufferOld  -- Palette RAM is not buffered
      advanceVRAMAddress
      return result
  in \case
  0x2002 -> readStatusReg
  0x2007 -> readDataReg
  ______ -> pure 0

--http://wiki.nesdev.com/w/index.php/PPU_scrolling

cpuWriteRegister :: Word16 -> Word8 -> Emulator ()
cpuWriteRegister addr val = let val16 = fromIntegral val in case addr of
  0x2000 -> do
    writeReg ppuCtrl val
    modifyReg pvtTempAddr $ \reg -> reg .&. 0xF3FF .|. (val16 .&. 3) `shiftL` 10

  0x2001 -> writeReg ppuMask val

  0x2005 -> do
    latch <- readReg pvtAddressLatch
    if latch == 0
    then do 
      writeReg pvtFineX (val .&. 0x7)
      modifyReg pvtTempAddr $ \reg -> reg .&. 0x1F .|. val16 `shiftR` 3
    else do
      modifyReg pvtTempAddr $ 
        \reg -> reg .&. 0xC1F .|. (val16 .&. 0x7) `shiftL` 12 .|. (val16 .&. 0xF8) `shiftL` 2
    modifyReg pvtAddressLatch complement

  0x2006 -> do
    latch <- readReg pvtAddressLatch
    if latch == 0
    then do  -- higher byte is being written
      modifyReg pvtTempAddr $ \reg -> reg .&. 0xFF .|. (val16 .&. 0x3F) `shiftL` 8
    else do  -- lower  byte is being written
      tempAddr <- readReg pvtTempAddr
      let result = (tempAddr .&. 0xFF00) .|. val16
      writeReg pvtTempAddr result
      writeReg pvtVRAMAddr result 
    modifyReg pvtAddressLatch complement

  0x2007 -> do
    pvtVRAMAddr <- readReg pvtVRAMAddr
    write pvtVRAMAddr val
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

setPixel :: Word8 -> Word8 -> Pixel -> Emulator ()
setPixel x y (r,g,b) = do
  screen <- accessScreen
  let offset = 256*3*fromIntegral y + fromIntegral x*3
  liftIO $ do
    VSM.write screen  offset      r
    VSM.write screen (offset + 1) g
    VSM.write screen (offset + 2) b

drawPatternTable = do
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
            setPixel (fromIntegral patterntab * 128 + fromIntegral(x*8 + (7 - col))) (58 + fromIntegral(y*8 + row)) color

drawPalette = do
  forM_ [0..7] $ \palette ->
    forM_ [0..3] $ \pixel ->
      forM_ [0..4] $ \i -> do
        setPixel (palette * 25 + pixel*5 + i) 233 (255, 255, 255)
        setPixel (palette * 25 + pixel*5 + i) 239 (255, 255, 255)
        forM_ [0..4] $ \j -> do
          color <- getColor palette pixel
          setPixel (palette * 25 + pixel*5 + i) (234 + j) color

visibleScanLine :: Word16 -> Emulator ()
visibleScanLine = \case
  1 -> do
    vramAddr <- readReg pvtVRAMAddr
    read (0x2000 .|. vramAddr .&. 0xFFF) >>= writeReg emuNextNT
  _ -> pure ()

preRenderScanLine :: Word16 -> Emulator ()
preRenderScanLine = \case 
  _ -> pure ()

incrementYPos :: Emulator ()
incrementYPos = pure ()

incrementXPos :: Emulator ()
incrementXPos = pure ()

incrementYScroll :: Emulator ()
incrementYScroll = pure ()

clock :: Emulator ()
clock = do
  cycle     <- readReg emuCycle
  scanline  <- readReg emuScanLine

  let 
    any = const True; is = (==)
    noop = pure ()
    or = liftA2 (||)
    between a b = liftA2 (&&) (a<=) (<=b)
    enterVerticalBlank = do
      ppuCtrl <- readReg ppuCtrl
      modifyReg ppuStatus (`setBit` 7)
      when (ppuCtrl `testBit` 7) $ sendInterrupt NMI
    exitVerticalBlank = modifyReg ppuStatus (`clearBit` 7)
    step = (cycle - 1) .&. 0x7
    rules = [
      -- cyc    scanl
        (is 0,  is 0,    modifyReg emuCycle (+1) >> clock),
        (is 0,  any,     noop),
        (is 256, between 0 239 `or` is 261, incrementYScroll),
        (is 257, between 0 239 `or` is 261, incrementXPos),
        (any,   between 0 239, visibleScanLine step),
        (is 1,  is 241,  enterVerticalBlank),
        (is 1,  is 261,  exitVerticalBlank >> preRenderScanLine step),
        (between 280 304, is 261, incrementYPos),
        (not.between 258 320, is 261, preRenderScanLine step)
      ]

  apply cycle scanline $ rules

  writeReg emuCycle    (if cycle    == 340 then 0 else cycle    + 1)
  writeReg emuScanLine (if scanline == 261 then 0 else scanline + 1)

  where 
    apply cycle scanline = foldr (\(cp, sp, action) next -> if cp cycle && sp scanline then action else next) $ pure ()

