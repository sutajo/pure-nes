module Nes.PPUEmulator where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Data.Vector.Storable.Mutable as VSM
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

readPPUComponent :: (PPU -> VUM.IOVector Word8) -> Word16 -> Emulator Word8
readPPUComponent component addr = useMemory (component . ppu) (`VUM.read` (fromIntegral addr))

writePPUComponent :: (PPU -> VUM.IOVector Word8) -> Word16 -> Word8 -> Emulator ()
writePPUComponent component addr val = useMemory (component . ppu) (\ind -> VUM.write ind (fromIntegral addr) val)

readPaletteIndices  = readPPUComponent  paletteIndices
writePaletteIndices = writePPUComponent paletteIndices
readNametable       = readPPUComponent  nametable
writeNametable      = writePPUComponent nametable

read :: Word16 -> Emulator Word8
read addr
  | addr <= 0x1FFF = ppuReadCartridge addr
  | addr <= 0x3EFF = readNametable $ (addr - 0x2000) `rem` (4 * 0x400)
  | addr <= 0x3FFF = readPaletteIndices ((addr - 0x3F00) `rem` 0x20)

write :: Word16 -> Word8 -> Emulator ()
write addr val
  | addr <= 0x1FFF = ppuWriteCartridge addr val
  | addr <= 0x3EFF = writeNametable ((addr - 0x2000) `rem` (4 * 0x400)) val
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
  forM_ [0x3F00,0x3F04..0x3F1F] $ \addr -> write addr 0
  forM_ [0x3F01,0x3F05..0x3F1F] $ \addr -> write addr 3
  forM_ [0x3F02,0x3F06..0x3F1F] $ \addr -> write addr 23
  forM_ [0x3F03,0x3F07..0x3F1F] $ \addr -> write addr 10
  forM_ [0..15] $ \y -> do
    let rowOffset = y * 256
    forM_ [0..15] $ \x -> do
      let offset = rowOffset + x*16
      forM_ [0..7] $ \row -> do
        tile_lsb <- read (offset + row)
        tile_msb <- read (offset + row + 8)
        forM_ [0..7] $ \col -> do
          let 
            c = fromIntegral col
            get a i = fromEnum $ a `testBit` i 
          let pixel = fromIntegral $ ((tile_msb `get` c) `shiftL` 1) .|. (tile_lsb `get` c)
          color <- getColor 0 pixel
          setPixel (fromIntegral(x*8 + (7 - col))) (fromIntegral(y*8 + row)) color