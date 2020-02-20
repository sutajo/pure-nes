{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module CPU.Quick.Nestest.LogParser (
  PPUState(..),
  logParser
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Nes.CPU6502

data PPUState = PPUState {
  cycle    :: Word, 
  scanline :: Word
} deriving (Eq, Show)

snapshotParser :: Parser (Word8, CpuSnapshot, PPUState)
snapshotParser = do
  pc'    <- hexadecimal
  count 2  space
  opcode <- hexadecimal
  count 40 anyChar
  a'  <- string "A:"  *> hexadecimal <* space
  x'  <- string "X:"  *> (hexadecimal :: Parser Word8) <* space
  y'  <- string "Y:"  *> hexadecimal <* space
  p'  <- string "P:"  *> hexadecimal <* space
  s'  <- string "SP:" *> hexadecimal <* space
  string "PPU:"
  cycle    <- skipSpace *> decimal <* char ','
  scanline <- skipSpace *> decimal <* space
  cyc' <- string "CYC:" *> decimal <* endOfLine
  pure (opcode, CpuSnapshot{..}, PPUState{..})

logParser :: Parser [(Word8, CpuSnapshot, PPUState, Int)]
logParser = do
  snaps <- some snapshotParser
  pure $ zipWith (\(op,cpu,ppu) ind -> (op,cpu,ppu,ind)) snaps [1..]  


