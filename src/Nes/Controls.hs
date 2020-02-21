module Nes.Controls (
  Controller(..),
  Input(..),
  Button(..),
  powerUp,
  processInput,
  read,
  write
) where

import Prelude hiding (read)
import Data.Word
import Data.Bits

-- http://wiki.nesdev.com/w/index.php/Standard_controller

data Button  
  =  A
  |  B
  |  Select
  |  Start
  |  Up
  |  Down
  |  Left
  |  Right
  deriving (Enum, Eq)

data Input 
  = Press Button
  | Release Button
  deriving (Eq)

data Controller = Controller {
    activeButtons :: !Word8,
    index         :: !Int
} deriving (Show)

modify :: (Word8 -> Int -> Word8) -> Button -> Controller -> Controller
modify f k c@Controller {activeButtons = btns} = c { activeButtons = btns `f` (fromEnum k) }

press :: Button -> Controller -> Controller
press = modify setBit

release :: Button -> Controller -> Controller
release = modify clearBit

processInput :: Input -> Controller -> Controller
processInput input = case input of
  Press   btn -> press btn
  Release btn -> release btn

powerUp :: Controller
powerUp =
  let 
    activeButtons = 0
    index = 0
  in Controller{..}

read :: Controller -> (Word8, Controller)
read c@Controller{..} = 
  let 
    byte = (activeButtons `shiftR` index) .&. 0x1
    newController = c {index = index + 1}
  in 
  if index == 8 
  then (1, c)
  else (byte, newController)

write :: Word8 -> Controller -> Controller
write byte c
  | byte `testBit` 0 = c {index = 0} 
  | otherwise = c

