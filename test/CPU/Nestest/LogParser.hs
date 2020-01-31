{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module CPU.Nestest.LogParser (
  logParser,
  snapshotParser
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Nes.CPU6502

snapshotParser :: Parser (Word8, CpuSnapshot)
snapshotParser = do
  spc <- hexadecimal
  count 2  space
  opcode <- hexadecimal
  count 40 anyChar
  sa  <- string "A:"  *> hexadecimal <* space
  sx  <- string "X:"  *> (hexadecimal :: Parser Word8) <* space
  sy  <- string "Y:"  *> hexadecimal <* space
  sp  <- string "P:"  *> hexadecimal <* space
  ss  <- string "SP:" *> hexadecimal <* space
  count 12 anyChar
  scyc <- string "CYC:" *> decimal <* endOfLine
  pure (opcode, CpuSnapshot{..})

logParser :: Parser [(Word8, CpuSnapshot, Int)]
logParser = do
  snaps <- some snapshotParser
  zipWithM (\(a,b) c -> pure (a,b,c)) snaps [1..]  


