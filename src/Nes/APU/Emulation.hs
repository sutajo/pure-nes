{-# LANGUAGE LambdaCase #-}

module Nes.APU.Emulation (
    clock,
    read,
    write,
    accurateMixer,
    linearApproxMixer
) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Prelude hiding (read)
import           Nes.APU.Memory              as APU
import           Nes.CPU.Memory
import           Nes.Emulation.Monad

type Clock = State APU

asNum :: Num a => Bool -> a
asNum True  = 1
asNum False = 0

selectPulse :: Functor f => Word16 -> (PulseWave -> f PulseWave) -> APU -> f APU
selectPulse addr
  | addr <= 0x3 = channels.chPulse0
  | otherwise   = channels.chPulse1

read :: Word16 -> APU -> Word8
read _ _ = 0

write :: Word16 -> Word8 -> APU -> APU
write addr byte apu = writeReg `execState` apu
  where
    writeReg :: Clock ()
    writeReg
      | addr <= 0x7 = writePulse
      | otherwise   = return ()
    writePulse :: Clock ()
    writePulse = let
        pulse :: Functor f => (PulseWave -> f PulseWave) -> APU -> f APU
        pulse = selectPulse addr
      in case addr .&. 3 of
        0 -> do
          pulse.duty               .= byte `shiftR` 6      
          pulse.loopHalt           .= byte `testBit` 5
          pulse.constVolume        .= byte `testBit` 4
          pulse.envelope.volume    .= byte .&. 0x0F
        1 -> do
          pulse.sweep.enabled      .= byte `testBit` 7
          pulse.sweep.period       .= (byte `shiftR` 4) .&. 0x7
          pulse.sweep.(APU.negate) .= byte `testBit` 3
          pulse.sweep.(APU.shift)  .= byte .&. 0x7
        2 -> do
          pulse.timer.counter      %= \c -> c .&. 0x700 .|. fromIntegral byte
        3 -> do
          pulse.timer.counter      %= \c -> c .&. 0xFF .|. (fromIntegral (byte .&. 7) `shiftL` 8)
          pulse.lengthCounter.load .= byte `shiftR` 3


accurateMixer :: ChannelSamples -> Double
accurateMixer ChannelSamples{..} = output
  where
    output = pulse_out + tnd_out
    pulse_out = 
      let pulseSum = pulse0 + pulse1 
      in if pulseSum == 0 
      then 0
      else 95.88 / (8128 / pulseSum + 100)
    tnd_out = 
      let sum = triangle / 8227 + noise / 12241 + dmc / 22638
      in if sum == 0
      then 0
      else 159.79 / (1 / sum + 100)

linearApproxMixer :: ChannelSamples -> Double
linearApproxMixer ChannelSamples{..} = output
  where
    output = pulse_out + tnd_out
    pulse_out = 0.00752 * (pulse0 + pulse1)
    tnd_out = 0.00851 * triangle + 0.00494 * noise + 0.00335 * dmc


clockEnvelope :: Clock ()
clockEnvelope = pure ()

clockLengthCounter :: Clock ()
clockLengthCounter = pure ()


sequencer :: Int -> Clock ()
sequencer stage = do
  frameCounter <- use frameCounter
  if frameCounter `testBit` 7
  then case stage of
    0 -> clockEnvelope
    1 -> clockEnvelope >> clockLengthCounter
    2 -> clockEnvelope
    3 -> pure ()
    4 -> clockEnvelope >> clockLengthCounter
  else case stage of
    0 -> clockEnvelope
    1 -> clockEnvelope >> clockLengthCounter
    2 -> clockEnvelope
    3 -> clockEnvelope >> clockLengthCounter -- >> when (not $ frameCounter `testBit` 6) sendIrq

type Output = (Maybe Interrupt, Maybe ChannelSamples)

clock :: APU -> Int -> (Output, APU)
clock oldState masterClocks = step `runState` oldState
  where
    step :: Clock Output
    step = do
      
      return (Nothing, Nothing)