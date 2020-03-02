module Nes.Oscillator where

import Data.Word
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

newtype Oscillator = Oscillator { 
  sample :: Double -> Word8
}

sineWaveGenerator :: Oscillator
sineWaveGenerator = Oscillator (\t -> round $ sin (t / 20) * 100)

plotWaveForm (Oscillator f) = do
  let signal points = [(x, (fromIntegral $ f x) :: Int) | x <- points]
  toFile def "waveform.png" $ do
    layout_title .= "Waveform plot"
    plot (line "am" [signal [0,(0.5)..400]])