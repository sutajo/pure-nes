{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module CPU.Quick.Nestest.LogParser (
  PPUState(..),
  logParser
) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Word
import           Nes.CPU.Serialization

data PPUState = PPUState {
  cycle    :: Int, 
  scanline :: Int
} deriving (Eq, Show)

snapshotParser :: Parser (Word8, CPU, PPUState)
snapshotParser = do
  let hex8  = hexadecimal :: Parser Word8
  let hex16 = hexadecimal :: Parser Word16
  pc    <- hex16
  count 2  space
  opcode <- hex8
  count 40 anyChar
  a  <- string "A:"  *> hex8 <* space
  x  <- string "X:"  *> hex8 <* space
  y  <- string "Y:"  *> hex8 <* space
  p  <- string "P:"  *> hex8 <* space
  s  <- string "SP:" *> hex8 <* space
  string "PPU:"
  cycle    <- skipSpace *> decimal <* char ','
  scanline <- skipSpace *> decimal <* space
  cyc <- string "CYC:" *> decimal <* endOfLine
  let irqTimer = 0
  let nmiTimer = 0
  pure (opcode, CPU{..}, PPUState{..})

logParser :: Parser [(Word8, CPU, PPUState, Int)]
logParser = do
  snaps <- some snapshotParser
  pure $ zipWith (\(op,cpu,ppu) ind -> (op,cpu,ppu,ind)) snaps [1..]  


