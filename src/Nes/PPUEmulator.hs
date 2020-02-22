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
  readReg,
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
import           Text.Printf
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
 | 0x10 <= addr && addr <= 0x1C && addr .&. 0b11 == 0 = usage (addr - 0x10)
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

increment :: (Prim a, Num a) => (PPU -> IORefU a) -> Emulator ()
increment = (`modifyReg` (+1))

advanceVRAMAddress = do
  ppuCtrl <- readReg ppuCtrl
  pvtVRamAddr $= (+ if ppuCtrl `testBit` 2 then 32 else 1)

getFrameCount = readReg emuFrameCount
getCoarsePosition = readReg pvtVRamAddr <&> (\v -> (v .&. 0x1F, (v `shiftR` 5) .&. 0x1F)) 

clearVblAfterStatusRead :: Emulator ()
clearVblAfterStatusRead = do
  ppuStatus $= (`clearBit` 7)
  transfer emuClocks emuLastStatusRead

debugging :: Bool
debugging = False

withInfo :: IO () -> Emulator ()
withInfo action = when debugging $ do
  vramAddr <- readReg pvtVRamAddr
  attrLo <- readReg emuAttrShifterLo
  attrHi <- readReg emuAttrShifterHi
  nextAT <- readReg emuNextAT
  cycle    <- readReg emuCycle
  scanLine <- readReg emuScanLine
  liftIO $ do
    putStr $ show (cycle, scanLine) ++ printf " - V: 0x%X" vramAddr ++ " -> "
    action

cpuReadRegister :: Word16 -> Emulator Word8
cpuReadRegister addr = 
  let
    readStatusReg = do
      result <- liftA2 (.|.) (readReg ppuStatus <&> (.&. 0xE0)) (readReg pvtDataBuffer <&> (.&. 0x1F))
      clearVblAfterStatusRead
      pvtAddressLatch .= 0
      return result
    readDataReg = do
      vramAddr  <- readReg pvtVRamAddr
      let paletteRead = vramAddr >= 0x3F00
      result <- if paletteRead
        then do
          read (vramAddr - 0x1000) >>= writeReg pvtDataBuffer
          read vramAddr
        else do
          (readReg pvtDataBuffer) <* (read vramAddr >>= writeReg pvtDataBuffer)
      advanceVRAMAddress
      return result
  in case addr of
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
    let shouldOccur = inVBlank && not occured && nmiOuput 
    when shouldOccur $ do
      setFlag emuNmiOccured True
      sendPendingNmi 2
    ppuCtrl .= val
    setFlag emuNmiOccured shouldOccur
    pvtTempAddr $= \reg -> (reg .&. 0x73FF) .|. ((val16 .&. 3) `shiftL` 10)

  0x2001 -> ppuMask .= val

  0x2005 -> do
    latch <- readReg pvtAddressLatch
    case latch of
      0 -> do
        pvtFineX    .=  val .&. 0x7
        pvtTempAddr $= \reg -> reg .&. 0x7FE0 .|. val16 `shiftR` 3
      _ -> do
        pvtTempAddr $= 
          \reg -> reg .&. 0xC1F .|. (val16 .&. 0x7) `shiftL` 12 .|. (val16 `shiftR` 3) `shiftL` 5
    pvtAddressLatch $= complement

  0x2006 -> do
    latch <- readReg pvtAddressLatch
    case latch of 
      0 -> do  -- higher byte is being written
        pvtTempAddr $= \reg -> reg .&. 0xFF .|. (val16 .&. 0x3F) `shiftL` 8
      _ -> do  -- lower  byte is being written
        tempAddr <- readReg pvtTempAddr
        let result = (tempAddr .&. 0xFF00) .|. val16
        pvtTempAddr .= result
        pvtVRamAddr .= result 
    pvtAddressLatch $= complement

  0x2007 -> do
    pvtVRamAddr <- readReg pvtVRamAddr
    write pvtVRamAddr val
    advanceVRAMAddress

  ______ -> pure ()

read :: Word16 -> Emulator Word8
read addr
  | addr <= 0x1FFF = ppuReadCartridge addr
  | addr <= 0x3EFF = do
    mirror <- getNametableMirroring
    readNametable (mirror addr)
  | addr <= 0x3FFF = readPaletteIndices ((addr - 0x3F00) .&. 0x1F)

write :: Word16 -> Word8 -> Emulator ()
write addr val
  | addr <= 0x1FFF = ppuWriteCartridge addr val
  | addr <= 0x3EFF = do
    mirror <- getNametableMirroring
    writeNametable (mirror addr) val
  | addr <= 0x3FFF = writePaletteIndices ((addr - 0x3F00) .&. 0x1F) val

getColor :: Word8 -> Word8 -> Emulator Pixel
getColor palette pixel = do
  index <- read (0x3F00 + fromIntegral (palette `shiftL` 2 + pixel))
  readPalette (index .&. 0x3F)

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
            x = fromIntegral (coarsex*8 + (7 - col))
            y = fromIntegral (coarsey*8 + row)
          color <- getColor attr pixel
          --liftIO $ putStrLn $ show (x,y) ++ " -> " ++ show (coarsex, coarsey, col, row, val, pixel, attr)
          setPixel x y color

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
    shiftMask <- readReg pvtFineX <&> (\reg -> 0x8000 `shiftR` (fromIntegral reg))
    pixelLo   <- readReg emuPattShifterLo <&> (.&. shiftMask)
    pixelHi   <- readReg emuPattShifterHi <&> (.&. shiftMask)
    paletteLo <- readReg emuAttrShifterLo <&> (.&. shiftMask)
    paletteHi <- readReg emuAttrShifterHi <&> (.&. shiftMask)
    let getBit index byte = fromEnum $ byte `testBit` index
    let getFirstBit16 = getBit 15 
    let pixel   = fromIntegral $ getFirstBit16 pixelHi   `shiftL` 1 .|. getFirstBit16 pixelLo
    let palette = fromIntegral $ getFirstBit16 paletteHi `shiftL` 1 .|. getFirstBit16 paletteLo
    cycle    <- readReg emuCycle
    scanLine <- readReg emuScanLine
    getColor palette pixel >>= setPixel (cycle - 1) scanLine


drawPixel :: Emulator ()
drawPixel = do
  drawBackgroundPixel


updateShiftregisters :: Emulator ()
updateShiftregisters = do
  at  <- readReg emuNextAT        
  lsb <- readReg emuNextLSB       
  msb <- readReg emuNextMSB
  let updatePatt lo shifter = shifter .|. fromIntegral lo
  let updateAttr lo shifter = shifter .|. (if lo /= 0 then 0xFF else 0x00)
  emuPattShifterLo $= (updatePatt lsb)
  emuPattShifterHi $= (updatePatt msb)
  emuAttrShifterLo $= (updateAttr (at .&. 0b01))
  emuAttrShifterHi $= (updateAttr (at .&. 0b10))


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

scanLine :: Int -> Emulator ()
scanLine step = case step of
  0 -> do
    updateShiftregisters
    v <- readReg pvtVRamAddr
    read (0x2000 .|. v .&. 0xFFF) >>= writeReg emuNextNT
  2 -> do
    v                  <- readReg pvtVRamAddr
    (coarseX, coarseY) <- getCoarsePosition
    attributeGroup <- read (0x23C0 .|. (v .&. 0xC00) .|. (coarseY .&. 0x1C) `shiftL` 1 .|. coarseX `shiftR` 2)
    let attr = attributeGroup `shiftR` (fromIntegral $ ((coarseY .&. 0b10) `shiftL` 1 .|. coarseX .&. 0b10))
    emuNextAT .= attr .&. 0b11
  4 -> do
    patternOffset <- readReg ppuCtrl   <&> \ctrl -> if ctrl `testBit` 4 then 0x1000 else 0
    ntId          <- readReg emuNextNT <&> fromIntegral
    v             <- readReg pvtVRamAddr
    let addr = patternOffset + ntId `shiftL` 4 + ((v `shiftR` 12) .&. 0b111)
    read addr     >>= writeReg emuNextLSB
  6 -> do
    patternOffset <- readReg ppuCtrl   <&> \ctrl -> if ctrl `testBit` 4 then 0x1000 else 0
    ntId          <- readReg emuNextNT <&> fromIntegral
    v             <- readReg pvtVRamAddr
    let addr = patternOffset + ntId `shiftL` 4 + ((v `shiftR` 12) .&. 0b111)
    read (addr+8) >>= writeReg emuNextMSB
  7 -> incrementHorizontal
  _ -> pure ()

isRenderingEnabled :: Emulator Bool
isRenderingEnabled = readReg ppuMask <&> (\mask -> mask .&. 0b00011000 /= 0)

whenRendering = whenM isRenderingEnabled

transferTempWithMask mask = do
  t <- readReg pvtTempAddr <&> (.&. mask)
  pvtVRamAddr $= \reg -> (reg .&. complement mask) .|. t

verticalMask = 0b111101111100000

transferVertical :: Emulator ()
transferVertical = whenRendering $ do
  transferTempWithMask verticalMask

transferHorizontal :: Emulator ()
transferHorizontal = whenRendering $ do
  transferTempWithMask (complement verticalMask)

incrementHorizontal :: Emulator ()
incrementHorizontal = whenRendering $ do
  coarseXwraps <- readReg pvtVRamAddr <&> (\reg -> (reg .&. 0x1F) == 0x1F)
  pvtVRamAddr $=
    if coarseXwraps
    then (\v -> (v .&. 0x7FE0) `xor` 0x400)
    else (+1)

incrementVertical :: Emulator ()
incrementVertical = whenRendering $ do
  vramAddr <- readReg pvtVRamAddr
  let canIncrementFineY = vramAddr .&. 0x7000 /= 0x7000
  if canIncrementFineY
  then do
    pvtVRamAddr $= (+0x1000)
  else do
    pvtVRamAddr $= (.&. 0x8FFF)
    let coarseY = (vramAddr .&. 0x3E0) `shiftR` 5
    case coarseY of
      29 -> (pvtVRamAddr $= (\addr -> (addr .&. 0xFC1F) `xor` 0x800))
      31 -> (pvtVRamAddr $= (.&. 0xFC1F))
      __ -> (pvtVRamAddr $= (+0x20))


moveToNextPosition :: Int -> Int -> Emulator ()
moveToNextPosition cycle scanline = do
  case cycle of
    340 -> do
      emuCycle .= 0
      case scanline of
        261 -> do
          frameCount <- readReg emuFrameCount
          let oddFrame = frameCount `testBit` 0
          rendering  <- isRenderingEnabled
          when (rendering && oddFrame) $ writeReg emuCycle 1 -- skip first cycle on odd frame
          emuScanLine .= 0
          increment emuFrameCount
        ___ -> increment emuScanLine
    ___ -> increment emuCycle


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
  writeReg emuScanLine   261
  writeReg emuPattShifterHi 1


clock :: Emulator ()
clock = do
  cycle     <- readReg emuCycle
  scanline  <- readReg emuScanLine

  let
    any = const True; is = (==)
    between a b = liftA2 (&&) (a<=) (<=b)
    step = (cycle - 1) .&. 0x7
    vBlank = between 240 260
    drawPixelIfVisible = when (between 1 256 cycle && between 0 239 scanline) drawPixel
    executeCycle = do
      when (between 2 258 cycle || between 321 337 cycle) shiftRegisters
      scanLine step
      drawPixelIfVisible
    phases = [
        --               cyc           scanl
        (               is 0,            any,  noop                                        ),
        (             is 256,     not.vBlank,  executeCycle >> incrementVertical           ),
        (             is 257,     not.vBlank,  transferHorizontal                          ),
        (               is 1,         is 241,  enterVerticalBlank                          ),
        (               is 1,         is 261,  exitVerticalBlank                           ),
        (not.between 258 320,     not.vBlank,  executeCycle                                ),
        (    between 280 304,         is 261,  transferVertical                            )
      ]

  pos <- getCoarsePosition
  withInfo $ print pos

  runPhase cycle scanline $ phases
  moveToNextPosition cycle scanline
  increment emuClocks

  where 
    noop = pure ()
    runPhase cycle scanline phases = do
      let match = find ( \(cp, sp, _) -> cp cycle && sp scanline ) phases
      maybe noop ( \(_,_,action) -> action ) match