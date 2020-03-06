module Nes.APU.Oscillator (
  Oscillator(..),
  sineWaveGenerator,
  pulseWaveGenerator,
  triangleWaveGenerator,
  sawtoothWaveGenerator
) where

import Data.Colour.Names
import Control.Applicative
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

newtype Oscillator = Oscillator { 
  sample :: Double -> Double
}

instance Num Oscillator where
  (+) (Oscillator f) (Oscillator g) = Oscillator $ liftA2 (+) f g
  (*) (Oscillator f) (Oscillator g) = Oscillator $ liftA2 (*) f g
  abs (Oscillator f) = Oscillator $ abs . f
  signum (Oscillator f) = Oscillator $ signum
  fromInteger x = Oscillator $ const (fromIntegral x)
  negate (Oscillator f) = Oscillator $ negate . f

instance Fractional Oscillator where
  (/) (Oscillator f) (Oscillator g) = Oscillator $ liftA2 (/) f g
  fromRational x = Oscillator $ const (fromRational x)

plotWaveForms oscillators = do
  let signal points (Oscillator f) = [(x, f x) | x <- points]
  toFile def "waveform" $ do
    setColors $ map opaque [blue, green, gold, red]
    layout_title  .= "Waveform plot"
    layout_x_axis .= def { _laxis_title = "t" }
    mapM_ (\(o, n) -> plot (line n [signal [0,0.5..400] o])) oscillators

plotWaveForm x = plotWaveForms [x]

sineWaveGenerator :: Oscillator
sineWaveGenerator = Oscillator (\t -> sin (t * 2 * pi / 100) * 100)

plotSine = plotWaveForm (sineWaveGenerator, "sin(t*2*pi/100)*100")

pulseWaveGenerator :: Double -> Double -> Double -> Int -> Oscillator
pulseWaveGenerator period pulseTime amplitude elementCount = Oscillator (\t -> (f t) * amplitude)
  where
    f t = dutyCycles + sum (map (element (t*2*pi/100)) [1..elementCount]) 
    dutyCycles = pulseTime / period
    element t n = let
      pin = pi * fromIntegral n
      in (2 * sin (pin * dutyCycles) * cos (2 * pin * dutyCycles * t))/ pin

plotPulseWave = plotWaveForm (pulseWaveGenerator 100 20 512 25, "Pulse wave")

sawtoothWaveGenerator period amplitude = Oscillator $ \t -> let tp = t/period in amplitude*2*(tp-fromIntegral (floor(tp+1/2)))

triangleWaveGenerator :: Double -> Double -> Oscillator
triangleWaveGenerator period = abs . sawtoothWaveGenerator period

plotTriangleWave = plotWaveForm (triangleWaveGenerator 100 200, "Triangle wave")

