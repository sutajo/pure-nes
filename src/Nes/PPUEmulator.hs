module Nes.PPUEmulator where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Word
import           Data.Vector.Storable.Mutable as VSM
import           Nes.PPU
import           Nes.EmulatorMonad

accessScreen :: Emulator (IOVector Word8)
accessScreen = useMemory (screen . ppu) $ pure

clock :: Emulator ()
clock = do
  screen <- accessScreen
  forM_ [0..256*3*120-1] $ \i -> liftIO $ write screen i 0
