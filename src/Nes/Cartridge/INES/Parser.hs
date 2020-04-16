{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveAnyClass #-}

module Nes.Cartridge.INES.Parser (
  Cartridge(..),
  Mirroring(..),
  Mapper(..),
  MapperState(..),
  dummyMapper,
  mappersById,
  loadCartridge
) where

-- INES format: https://wiki.nesdev.com/w/index.php/INES
-- https://formats.kaitai.io/ines/index.html 

import           Control.Exception
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Map                    as M
import           Data.Functor
import           Data.Word
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString      as BS hiding (readFile, putStrLn, map, notElem) 
import qualified Data.ByteString.Lazy as B (readFile, toStrict)
import           Control.Monad
import           Nes.Cartridge.Memory
import           Nes.Cartridge.Mappers


data CartridgeException
  = UnsupportedMapper Word8
  | ParsingFailed String
  deriving (Exception)


instance Show CartridgeException where
  show = \case
    UnsupportedMapper id -> "Mapper " ++ show id ++ " is currently not supported."
    ParsingFailed msg -> msg


prg_rom_page_size = 16384
chr_rom_page_size = 8192
prg_ram_page_size = 8192


header :: Get [Word8]
header = replicateM 7 getWord8


iNESloader :: Get INES
iNESloader = do
  magicNumbersMatch <- getByteString 4 <&> (== "NES\SUB")
  when (not magicNumbersMatch) $ fail $ unlines ["iNES magic numbers are missing.", "Make sure you selected a cartridge file."]
  [  len_prg_rom , len_chr_rom 
   , flags6      , flags7      
   , len_prg_ram , flags9      
   , _ ] <- header   
  skip 5
  let 
    has_trainer = testBit flags6 2
    has_playChoice = testBit flags7 0
    prg_ram_size = (prg_ram_page_size * fromIntegral len_prg_ram)
  when (flags9 `testBit` 0) $ fail "This emulator can only handle NTSC ROMs." 
  when (has_trainer) $ skip 512      --We are skipping trainers for now
  prg_rom_bs  <- getByteString (prg_rom_page_size * fromIntegral len_prg_rom)
  chr_rom_bs  <- getByteString (chr_rom_page_size * fromIntegral len_chr_rom)
  when has_playChoice (skip 8224)
  title       <- B.toStrict <$> getRemainingLazyByteString
  return INES{..}
    

tryLoadingINES :: FilePath -> IO INES
tryLoadingINES path = do
    contents <- B.readFile path
    case runGetOrFail iNESloader contents of
      Left (_,_,err) -> do
        throw (ParsingFailed err)
      Right (_,_,cartridge) -> 
        pure cartridge 


toVector :: ByteString -> IO (VUM.IOVector Word8)
toVector bs = VU.thaw $ VU.fromList (BS.unpack bs)


assembleCartridge :: INES -> IO Cartridge
assembleCartridge INES{..} = do
  let 
    mapperId = (flags6 `shiftR` 4) .|. (flags7 .&. 0xF0)
    mirror = if flags6 `testBit` 3 then FourScreen else (toEnum . fromEnum) (flags6 `testBit` 0)
  when (mapperId `M.notMember` mappersById) $ throw (UnsupportedMapper mapperId)
  let hasChrRam = BS.length chr_rom_bs == 0
  chr_rom <- if hasChrRam then VUM.new 0x2000 else toVector chr_rom_bs
  prg_rom <- toVector prg_rom_bs
  prg_ram <- VUM.new (if prg_ram_size == 0 then 0x2000 else fromIntegral prg_ram_size)
  let 
    cart = Cartridge{..}
    mapper = dummyMapper
  assembledMapper <- (mappersById M.! mapperId) cart
  pure cart { mapper = assembledMapper }


loadCartridge :: FilePath -> IO Cartridge
loadCartridge path = tryLoadingINES path >>= assembleCartridge





