{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module CPU.Quick.Nestest.LogParser (
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
  pc'    <- hexadecimal
  count 2  space
  opcode <- hexadecimal
  count 40 anyChar
  a'  <- string "A:"  *> hexadecimal <* space
  x'  <- string "X:"  *> (hexadecimal :: Parser Word8) <* space
  y'  <- string "Y:"  *> hexadecimal <* space
  p'  <- string "P:"  *> hexadecimal <* space
  s'  <- string "SP:" *> hexadecimal <* space
  count 12 anyChar
  cyc' <- string "CYC:" *> decimal <* endOfLine
  pure (opcode, CpuSnapshot{..})

logParser :: Parser [(Word8, CpuSnapshot, Int)]
logParser = do
  snaps <- some snapshotParser
  pure $ zipWith (\(a,b) c -> (a,b,c)) snaps [1..]  


