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

newtype Oscillator a = Oscillator { 
  sample :: Double -> a
}

instance Num a => Num (Oscillator a) where
  (+) (Oscillator f) (Oscillator g) = Oscillator $ liftA2 (+) f g
  (*) (Oscillator f) (Oscillator g) = Oscillator $ liftA2 (*) f g
  abs (Oscillator f) = Oscillator $ abs . f
  signum _ = undefined
  fromInteger x = Oscillator $ const (fromIntegral x)
  negate (Oscillator f) = Oscillator $ negate . f

instance Fractional a => Fractional (Oscillator a) where
  (/) (Oscillator f) (Oscillator g) = Oscillator $ liftA2 (/) f g
  fromRational x = Oscillator $ const (fromRational x)

plotWaveForms oscillators = do
  let signal points (Oscillator f) = [(x, f x) | x <- points]
  toFile def "waveform" $ do
    setColors $ map opaque [blue, green, gold, red]
    layout_title  .= "Waveform plot"
    layout_x_axis .= def { _laxis_title = "t" }
    mapM_ (\(o, n) -> plot (line n [signal [0,0.05..30] o])) oscillators

plotWaveForm x = plotWaveForms [x]

sineWaveGenerator :: Oscillator Double
sineWaveGenerator = Oscillator sin

plotSine = plotWaveForm (sineWaveGenerator, "sin(t)")

pulseWaveGenerator :: Double -> Double -> Double -> Int -> Oscillator Double
pulseWaveGenerator period pulseTime amplitude elementCount = Oscillator (\t -> (f t) * amplitude)
  where
    f t = dutyCycles + sum (map (element t) [1..elementCount]) 
    dutyCycles = pulseTime / period
    element t n = let
      pin = pi * fromIntegral n
      pinDuty = pin * dutyCycles
      in (2 * sin pinDuty * cos (2 * pinDuty * t))/ pin

plotPulseWave = plotWaveForm ((pulseWaveGenerator (2*pi) pi 1 25 - 0.5) * 1.675, "Pulse wave")

sawtoothWaveGenerator period amplitude = Oscillator $ \t -> let tp = t/period in amplitude*2*(tp-fromIntegral (floor(tp+1/2)))

triangleWaveGenerator :: Double -> Double -> Oscillator Double
triangleWaveGenerator period = abs . sawtoothWaveGenerator period

plotTriangleWave = plotWaveForm (triangleWaveGenerator 100 200, "Triangle wave")

-- Converts an analog singal between [-a..a] to a digital one between [-a*n..a*n]
(~>) :: Integral a => Oscillator Double -> a -> Oscillator a
(~>) (Oscillator f) n = Oscillator $ \t -> round (f t * fromIntegral n)

plotWaves = let
  p = pulseWaveGenerator (2*pi) pi 1 10
  s = sineWaveGenerator
  in plotWaveForms [(p, "Pulse wave"), (s, "Sine wave"), (p+s, "Sum")]

