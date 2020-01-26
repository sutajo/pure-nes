 module Nes.Controls (
   Controller(..),
   UserInput(..),
   Button(..)
 ) where

import Data.Word
import Data.Bits

data Button
  =  Select
  |  Start
  |  Up
  |  Down
  |  Left
  |  Right
  |  A
  |  B
  deriving (Enum)

data UserInput 
  = Press Button
  | Release Button

newtype Controller = Controller {
    activeButtons :: Word8
}

modify :: (Word8 -> Int -> Word8) -> Button -> Controller -> Controller
modify f k (Controller s) = Controller (s `f` (fromEnum k))

press :: Button -> Controller -> Controller
press = modify setBit

release :: Button -> Controller -> Controller
release = modify clearBit

isPressed :: Button -> Controller -> Bool
isPressed btn (Controller s) = s `testBit` fromEnum btn 
