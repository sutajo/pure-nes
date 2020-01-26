{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, LambdaCase, FlexibleContexts #-}

module Nes.Cartridge (
  Cartridge(..),
  initFrom
) where

-- INES format: https://wiki.nesdev.com/w/index.php/INES
-- https://formats.kaitai.io/ines/index.html 

import           Prelude hiding (init)
import           Data.Array.IO
import           Data.Word
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString      as BS hiding (readFile, init, putStrLn) 
import qualified Data.ByteString.Lazy as B (readFile, toStrict)
import           Control.Monad
import           Control.Applicative()
import           Control.Exception

data INES = INES {
    title         :: ByteString,
    flags6        :: Word8,
    flags7        :: Word8,
    flags9        :: Word8,
    chr_rom_bs    :: ByteString,
    prg_rom_bs    :: ByteString,
    prg_ram_size  :: Int 
} deriving (Show)

-- https://wiki.nesdev.com/w/index.php/Mapper

prg_rom_page_size = 16384
chr_rom_page_size = 8192
prg_ram_page_size = 8192

header :: Get [Word8]
header = replicateM 7 getWord8
  
loader :: Get INES
loader = do
  "NES\SUB"   <- getByteString 4
  [  len_prg_rom , len_chr_rom 
   , flags6      , flags7      
   , len_prg_ram , flags9      
   , flags10 ] <- header   
  "\NUL\NUL\NUL\NUL\NUL" <- getByteString 5
  let 
    has_trainer = testBit flags6 2
    has_playChoice = testBit flags7 0
    prg_ram_size = (prg_ram_page_size * fromIntegral len_prg_ram)
  when (flags9 `testBit` 0) $ fail "This emulator can only handle NTSC ROMs." 
  when (has_trainer) $ skip 512      --We are skipping trainers for now
  prg_rom_bs  <- getByteString (prg_rom_page_size * fromIntegral len_prg_rom)
  chr_rom_bs  <- getByteString (chr_rom_page_size * fromIntegral len_chr_rom)
  playChoice  <- when has_playChoice (skip 8224)
  title       <- B.toStrict <$> getRemainingLazyByteString
  return INES{..}
    

tryLoadingINES :: FilePath -> IO INES
tryLoadingINES path = do
    contents <- B.readFile path
    case runGetOrFail loader contents of
      Left (_,_,err) -> do
        fail (show err)
      Right (_,_,cartridge) -> 
        pure cartridge 

data Mirroring
  = Horizontal
  | Vertical
  | FourScreen
  deriving (Show, Enum)
    
data Cartridge = Cartridge {
    mapper       :: Word8,
    mirror       :: Mirroring,
    chr_rom      :: IOUArray Word32 Word8,
    prg_rom      :: IOUArray Word32 Word8,
    prg_ram      :: IOUArray Word32 Word8
}

init :: INES -> IO Cartridge
init INES{..} = do
  let 
    mapper = (flags6 `shiftR` 4) .|. ((flags7 `shiftR` 4) `shiftL` 4)
    mirror = if flags6 `testBit` 3 then FourScreen else (toEnum . fromEnum) (flags6 `testBit` 0)
  when (mapper /= 0) . fail $ "Mapper type " ++ show mapper ++ " is currently not supported" 
  chr_rom <- newListArray (0, (fromIntegral $ BS.length chr_rom_bs)) $ unpack chr_rom_bs
  prg_rom <- newListArray (0, (fromIntegral $ BS.length prg_rom_bs)) $ unpack prg_rom_bs
  prg_ram <- newArray (0, (fromIntegral prg_ram_size)) 0
  return Cartridge{..}


initFrom :: FilePath -> IO Cartridge
initFrom path = tryLoadingINES path >>= init

