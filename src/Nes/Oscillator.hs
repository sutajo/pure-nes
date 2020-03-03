module Nes.Oscillator where

import Data.Int
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

newtype Oscillator = Oscillator { 
  sample :: Double -> Int16
}

plotWaveForm (Oscillator f) functionName = do
  let signal points = [(x, (fromIntegral $ f x) :: Int) | x <- points]
  toFile def "waveform" $ do
    layout_title  .= "Waveform plot"
    layout_x_axis .= def { _laxis_title = "t" }
    plot (line functionName [signal [0,0.5..400]])

sineWaveGenerator :: Oscillator
sineWaveGenerator = Oscillator (\t -> round $ sin (t * 2 * pi / 100) * 100)

plotSine = plotWaveForm sineWaveGenerator "sin(t*2*pi/100)*100"

pulseWaveGenerator :: Double -> Double -> Double -> Int -> Oscillator
pulseWaveGenerator period pulseTime amplitude elementCount = Oscillator (\t -> round $ (f t) * amplitude)
  where
    f t = dutyCycles + sum (map (element (t*2*pi/100)) [1..elementCount]) 
    dutyCycles = pulseTime / period
    element t n = let
      pin = pi * fromIntegral n
      in (2 * sin (pin * dutyCycles) * cos (2 * pin * dutyCycles * t))/ pin

plotPulseWave = plotWaveForm (pulseWaveGenerator 100 20 512 25) "Pulse wave"

triangleWaveGenerator :: Double -> Double -> Oscillator
triangleWaveGenerator period amplitude = Oscillator $ \t -> let tp = t/period in round $ amplitude*2*abs(tp-fromIntegral (floor(tp+1/2)))

plotTriangleWave = plotWaveForm (triangleWaveGenerator 50 100) "Triangle wave"

