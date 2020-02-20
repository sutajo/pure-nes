{-# LANGUAGE LambdaCase #-}

module Nes.PPUEmulator (
  reset,
  clock,
  accessScreen,
  cpuReadRegister,
  cpuWriteRegister,
  drawPatternTable,
  drawPalette,
  drawBackground,
  getFrameCount,
  read,
  write,
  readReg,
  writeReg
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Primitive(Prim)
import           Data.List (find)
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.IORef.Unboxed
import           Data.Functor
import           Data.Word
import           Data.Bits
import           Prelude hiding (read)
import           Nes.PPU
import           Nes.EmulatorMonad

--http://wiki.nesdev.com/w/index.php/PPU_registers
--http://wiki.nesdev.com/w/index.php/PPU_scrolling

--http://wiki.nesdev.com/w/index.php/PPU_palettes
--Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C

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

transfer source dest = readReg source >>= writeReg dest 

(.=) :: Prim a => (PPU -> IORefU a) -> a -> Emulator ()
(.=) = writeReg

infix 0 .=

modifyReg :: Prim a => (PPU -> IORefU a) -> (a -> a) -> Emulator ()
modifyReg reg f = readReg reg >>= writeReg reg . f

($=) :: Prim a => (PPU -> IORefU a) -> (a -> a) -> Emulator ()
($=) = modifyReg

infix 0 $=

testFlag :: (PPU -> Register8) -> Emulator Bool
testFlag reg = readReg reg <&> (>0)

setFlag :: (PPU -> Register8) -> Bool -> Emulator ()
setFlag reg val = reg .= (toEnum.fromEnum $ val)

increment = (`modifyReg` (+1))

advanceVRAMAddress = do
  ppuCtrl <- readReg ppuCtrl
  pvtVRAMAddr $= (+ if ppuCtrl `testBit` 2 then 32 else 1)

getFrameCount = readReg emuFrameCount

clearVblAfterStatusRead :: Emulator ()
clearVblAfterStatusRead = do
  ppuStatus $= (`clearBit` 7)
  transfer emuClocks emuLastStatusRead

cpuReadRegister :: Word16 -> Emulator Word8
cpuReadRegister = 
  let
    readStatusReg = do
      result <- liftA2 (.|.) (readReg ppuStatus <&> (.&. 0xE0)) (readReg pvtDataBuffer <&> (.&. 0x1F))
      clearVblAfterStatusRead
      pvtAddressLatch .= 0
      return result
    readDataReg = do
      vramAddr  <- readReg pvtVRAMAddr
      let paletteRead = vramAddr >= 0x3F00
      result <- if paletteRead
        then do
          read (vramAddr - 0x1000) >>= writeReg pvtDataBuffer
          read vramAddr
        else do
          (readReg pvtDataBuffer) <* (read vramAddr >>= writeReg pvtDataBuffer)
      advanceVRAMAddress
      return result
  in \case
  0x2002 -> readStatusReg
  0x2007 -> readDataReg
  ______ -> pure 0

cpuWriteRegister :: Word16 -> Word8 -> Emulator ()
cpuWriteRegister addr val = let val16 = fromIntegral val in case addr of
  0x2000 -> do
    status  <- readReg ppuStatus
    occured <- testFlag emuNmiOccured
    let nmiOuput = val `testBit` 7
    let inVBlank = status `testBit` 7 
    when (inVBlank && not occured && nmiOuput) $ do
      setFlag emuNmiOccured True
      sendPendingNmi 2
    ppuCtrl .= val
    setFlag emuNmiOccured nmiOuput
    pvtTempAddr $= \reg -> reg .&. 0xF3FF .|. (val16 .&. 3) `shiftL` 10

  0x2001 -> ppuMask .= val

  0x2005 -> do
    latch <- readReg pvtAddressLatch
    if latch == 0
    then do 
      pvtFineX    .=  val .&. 0x7
      pvtTempAddr $= \reg -> reg .&. 0x1F .|. val16 `shiftR` 3
    else do
      pvtTempAddr $= 
        \reg -> reg .&. 0xC1F .|. (val16 .&. 0x7) `shiftL` 12 .|. (val16 .&. 0xF8) `shiftL` 2
    pvtAddressLatch $= complement

  0x2006 -> do
    latch <- readReg pvtAddressLatch
    if latch == 0
    then do  -- higher byte is being written
      pvtTempAddr $= \reg -> reg .&. 0xFF .|. (val16 .&. 0x3F) `shiftL` 8
    else do  -- lower  byte is being written
      tempAddr <- readReg pvtTempAddr
      let result = (tempAddr .&. 0xFF00) .|. val16
      pvtTempAddr .= result
      pvtVRAMAddr .= result 
    pvtAddressLatch $= complement

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
    readNametable (mirror addr)
  | addr <= 0x3FFF = readPaletteIndices ((addr - 0x3F00) `rem` 0x20)

write :: Word16 -> Word8 -> Emulator ()
write addr val
  | addr <= 0x1FFF = ppuWriteCartridge addr val
  | addr <= 0x3EFF = do
    mirror <- getNametableMirroring
    writeNametable (mirror addr) val
  | addr <= 0x3FFF = writePaletteIndices ((addr - 0x3F00) `rem` 0x20) val

getColor :: Word8 -> Word8 -> Emulator Pixel
getColor palette pixel = do
  index <- read (0x3F00 + fromIntegral (palette `shiftL` 2 + pixel))
  readPalette index

setPixel :: Int -> Int -> Pixel -> Emulator ()
setPixel x y (r,g,b) = do
  screen <- accessScreen
  let offset = (256*y + x) * 3
  liftIO $ do
    VSM.write screen  offset      r
    VSM.write screen (offset + 1) g
    VSM.write screen (offset + 2) b

drawBackground :: Emulator ()
drawBackground = do
  ctrl <- readReg ppuCtrl
  let baseNametableAddr = (fromIntegral ctrl .&. 0b11) * 0x400
  let basePattAddr = (fromIntegral ctrl `shiftR` 4 .&. 0b1) * 0x1000
  screen <- accessScreen
  liftIO $ VSM.set screen 0
  forM_ [0..29] $ \coarsey -> do
    forM_ [0..31] $ \coarsex -> do
      val      <- read (0x2000 + baseNametableAddr + coarsey * 32 + coarsex)
      attrbyte <- read (0x2000 + baseNametableAddr + 0x3C0 + ((coarsey `shiftR` 2)*8 + coarsex `shiftR` 2))
      let attr = (attrbyte `shiftR` (fromIntegral $ ((coarsey .&. 0b10)*2 + coarsex .&. 0b10))) .&. 0b11
      forM_ [0..7] $ \row -> do
        forM_ [0..7] $ \col -> do
          let pattOffset = fromIntegral val*16
          tile_lsb <- read (basePattAddr+pattOffset+row)
          tile_msb <- read (basePattAddr+pattOffset+row+8)
          let 
            c = fromIntegral col
            get a i = fromEnum $ a `testBit` i 
            pixel = fromIntegral $ ((tile_msb `get` c) `shiftL` 1) .|. (tile_lsb `get` c)
          color <- getColor attr pixel
          setPixel (fromIntegral(coarsex*8 + (7 - col))) (fromIntegral(coarsey*8 + row)) color

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
          color <- getColor (fromIntegral palette) (fromIntegral pixel)
          setPixel (palette * 25 + pixel*5 + i) (234 + j) color


drawBackgroundPixel :: Emulator ()
drawBackgroundPixel = do
  mask <- readReg ppuMask
  when (mask `testBit` 3) $ do
    shiftMask <- readReg pvtFineX <&> (fromIntegral)
    pixelLo   <- readReg emuPattShifterLo <&> (.&. shiftMask)
    pixelHi   <- readReg emuPattShifterHi <&> (.&. shiftMask)
    paletteLo <- readReg emuAttrShifterLo <&> (.&. shiftMask)
    paletteHi <- readReg emuAttrShifterHi <&> (.&. shiftMask)
    let pixel = fromIntegral $ pixelHi `shiftR` 14 .|. pixelLo `shiftR` 15
    let palette = fromIntegral $ paletteHi `shiftR` 14 .|. paletteLo `shiftR` 15
    cycle    <- readReg emuCycle <&> fromIntegral
    scanLine <- readReg emuScanLine <&> fromIntegral
    getColor palette pixel >>= setPixel (cycle - 1) scanLine

drawPixel :: Emulator ()
drawPixel = do
  drawBackgroundPixel

updateShiftregisters :: Emulator ()
updateShiftregisters = do
  at  <- readReg emuNextAT        
  lsb <- readReg emuNextLSB       
  msb <- readReg emuNextMSB
  let updatePatt byte lo = byte .|. fromIntegral lo
  let updateAttr byte lo = byte .|. (if lo /= 0 then 0xFF else 0x00)
  emuPattShifterLo $= (`updatePatt` lsb)
  emuPattShifterHi $= (`updatePatt` msb)
  emuAttrShifterLo $= (`updateAttr` (at .&. 0b01))
  emuAttrShifterHi $= (`updateAttr` (at .&. 0b10))

shiftRegisters :: Emulator ()
shiftRegisters = do
  forM_ [
      emuPattShifterLo,
      emuPattShifterHi
    ] $ (`modifyReg` (`shiftL` 1))
  forM_ [
      emuAttrShifterLo,
      emuAttrShifterHi
    ] $ (`modifyReg` (`shiftL` 1))

scanLine :: Word -> Emulator ()
scanLine = \case
  0 -> do
    updateShiftregisters
    v <- readReg pvtVRAMAddr
    read (0x2000 .|. v .&. 0xFFF) >>= writeReg emuNextNT
  2 -> do
    v <- readReg pvtVRAMAddr
    writeReg emuNextAT =<< read (0x2C30 .|. (v .&. 0x0C00) .|. ((v `shiftR` 4) .&. 0x38) .|. ((v `shiftR` 2) .&. 0x7))
  4 -> do
    patternOffset <- readReg ppuCtrl   <&> ((`shiftL` 9) . (0b1000 .&.) . fromIntegral)
    nt            <- readReg emuNextNT <&> fromIntegral
    v             <- readReg pvtVRAMAddr
    let addr = patternOffset + nt `shiftL` 4 + v `shiftR` 12
    read addr     >>= writeReg emuNextLSB
    read (addr+8) >>= writeReg emuNextMSB
  7 -> incrementHorizontal
  _ -> pure ()

isRenderingEnabled :: Emulator Bool
isRenderingEnabled = readReg ppuMask <&> (\mask -> mask .&. 0b00011000 /= 0)

whenRendering = whenM isRenderingEnabled

transferTempWithMask mask = do
  t <- readReg pvtTempAddr <&> (.&. complement mask)
  modifyReg pvtVRAMAddr $ ((t .|.) . (mask .&.))

verticalMask = 0b111101111100000

transferVertical :: Emulator ()
transferVertical = whenRendering $ do
  transferTempWithMask verticalMask

transferHorizontal :: Emulator ()
transferHorizontal = whenRendering $ do
  transferTempWithMask (complement verticalMask)

incrementHorizontal :: Emulator ()
incrementHorizontal = whenRendering $ do
  coarseXwraps <- readReg pvtVRAMAddr <&> ((31 ==) . (0x1F .&.))
  modifyReg pvtVRAMAddr $ 
    if coarseXwraps
    then (\v -> (v .&. complement 0x001F) `xor` 0x0400)
    else (+1)

incrementVertical :: Emulator ()
incrementVertical = whenRendering $ do
  fineYwraps <- readReg pvtVRAMAddr <&> ((0x7000 /=) . (0x7000 .&.))
  modifyReg pvtVRAMAddr $ 
    if fineYwraps
    then (+0x1000)
    else \v -> 
      let
        v' = v .&. complement 0x7000
        y' = (v' .&. 0x3E0) `shiftR` 5
        (v'', y'') = case y' of 
          29 -> (v' `xor` 0x800, 0)
          31 -> (v',             0)
          __ -> (v',          y'+1)
      in (v'' .&. complement 0x3E0) .|. (y'' `shiftL` 5)


moveToNextPosition :: Word -> Word -> Emulator ()
moveToNextPosition cycle scanline = do
  if cycle == 340 
  then do
    emuCycle .= 0
    if scanline == 261
    then do
      frameCount <- readReg emuFrameCount
      let oddFrame = frameCount `rem` 2 == 1
      rendering <- isRenderingEnabled
      when (rendering && oddFrame) $ writeReg emuCycle 1
      emuScanLine .= 0
      increment emuFrameCount
    else
      increment emuScanLine
  else 
    increment emuCycle


enterVerticalBlank :: Emulator ()
enterVerticalBlank = do
  lastStatusRead <- readReg emuLastStatusRead
  currentClock   <- readReg emuClocks 
  when (lastStatusRead /= currentClock) $ do
    modifyReg ppuStatus (`setBit` 7) 
    ppuCtrl <- readReg ppuCtrl
    when (ppuCtrl `testBit` 7) $ do
      setFlag emuNmiOccured True
      sendNmi


exitVerticalBlank :: Emulator ()
exitVerticalBlank = do
  modifyReg ppuStatus (`clearBit` 7)
  setFlag emuNmiOccured False


reset :: Emulator ()
reset = do
  writeReg emuCycle      0
  writeReg emuScanLine   0


clock :: Emulator ()
clock = do
  cycle     <- readReg emuCycle
  scanline  <- readReg emuScanLine

  let
    any = const True; is = (==)
    between a b = liftA2 (&&) (a<=) (<=b)
    step = (cycle - 1) `rem` 8
    vBlank = between 240 260
    drawPixelIfVisible = when (between 1 256 cycle && between 0 239 scanline) drawPixel
    executeCycle = scanLine step >> drawPixelIfVisible >> shiftRegisters
    phases = [
        --               cyc           scanl
        (               is 1,         is 261,  exitVerticalBlank                   ),
        (               is 0,            any,  noop                                ),
        (             is 256,     not.vBlank,  executeCycle >> incrementVertical   ),
        (             is 257,     not.vBlank,  transferHorizontal                  ),
        (               is 1,         is 241,  enterVerticalBlank                  ),
        (not.between 258 320,     not.vBlank,  executeCycle                        ),
        (    between 280 304,         is 261,  transferVertical                    )
      ]

  runPhase cycle scanline $ phases
  moveToNextPosition cycle scanline
  increment emuClocks

  where 
    noop = pure ()
    runPhase cycle scanline phases = do
      let match = find ( \(cp, sp, _) -> cp cycle && sp scanline ) phases
      maybe noop ( \(_,_,action) -> action ) match