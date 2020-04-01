{-# LANGUAGE OverloadedLists, TemplateHaskell, DeriveAnyClass #-}

module Nes.APU.Memory where

import           Lens.Micro.Platform
import qualified Data.Vector.Unboxed          as VU
import           Data.Word
import           Data.Serialize
import           GHC.Generics

data ChannelSamples = ChannelSamples {
    pulse0   :: !Double,
    pulse1   :: !Double,
    triangle :: !Double,
    noise    :: !Double,
    dmc      :: !Double
} deriving (Generic, Serialize)

newtype Envelope = Envelope {
    _volume :: Word8
} deriving (Generic, Serialize)
makeLenses ''Envelope

data Sweep = Sweep {
    _enabled :: Bool,
    _period  :: Word8,
    _negate  :: Bool,
    _shift   :: Word8
} deriving (Generic, Serialize)
makeLenses ''Sweep

newtype Timer = Timer {
    _counter :: Word16
} deriving (Generic, Serialize)
makeLenses ''Timer

newtype LengthCounter = LengthCounter {
    _load :: Word8
} deriving (Generic, Serialize)
makeLenses ''LengthCounter

data PulseWave = PulseWave {
    _duty          :: Word8,
    _loopHalt      :: Bool,
    _constVolume   :: Bool,
    _envelope      :: Envelope,
    _sweep         :: Sweep,
    _timer         :: Timer,
    _lengthCounter :: LengthCounter
} deriving (Generic, Serialize)
makeLenses ''PulseWave

data Channels = Channels {
    _chPulse0 :: !PulseWave,
    _chPulse1 :: !PulseWave
} deriving (Generic, Serialize)
makeLenses ''Channels

data APU = APU {
    _samples        :: ChannelSamples,
    _channels       :: Channels,
    _frameCounter   :: Word8,
    _sampleTimer    :: Int
} deriving (Generic, Serialize)
makeLenses ''APU

powerUp :: APU
powerUp = let
      _duty          = 0
      _loopHalt      = False
      _constVolume   = False
      _envelope      = Envelope 0
      _timer         = Timer 0
      _lengthCounter = LengthCounter 0
      _sweep         = Sweep False 0 False 0
      _chPulse0      = PulseWave{..}
      _chPulse1      = PulseWave{..}
      _samples       = ChannelSamples 0 0 0 0 0
      _channels      = Channels{..}
      _frameCounter  = 0
      _sampleTimer   = 0
    in APU{..}

lengthTable :: VU.Vector Word8
lengthTable = 
    [10,254, 20,  2, 40,  4, 80,  6,160,  8, 60, 10, 14, 12, 26, 14,
     12, 16, 24, 18, 48, 20, 96, 22,192, 24, 72, 26, 16, 28, 32, 30]