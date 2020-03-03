{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, LambdaCase, FlexibleContexts, TypeFamilies #-}

module Nes.Cartridge (
  Cartridge,
  Mirroring(..),
  loadCartridge,
  cpuReadCartridge,
  cpuWriteCartridge,
  ppuReadCartridge,
  ppuWriteCartridge,
  mirror
) where

-- INES format: https://wiki.nesdev.com/w/index.php/INES
-- https://formats.kaitai.io/ines/index.html 

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Char (toUpper)
import qualified Data.Map                    as M
import           Data.Functor
import           Data.Word
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString      as BS hiding (readFile, putStrLn, map, notElem) 
import qualified Data.ByteString.Lazy as B (readFile, toStrict)
import           Control.Monad
import           Control.Applicative()
import           Numeric (showHex)

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
  
iNESloader :: Get INES
iNESloader = do
  magicNumbersMatch <- getByteString 4 <&> (== "NES\SUB")
  when (not magicNumbersMatch) $ fail "INES magic numbers are missing."
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
        fail (show err)
      Right (_,_,cartridge) -> 
        pure cartridge 

data Mirroring
  = Horizontal
  | Vertical
  | FourScreen
  deriving (Show, Enum)
    
data Cartridge = Cartridge {
    mapper       :: Mapper,
    mirror       :: Mirroring,
    chr_rom      :: VM.IOVector Word8,
    prg_rom      :: VM.IOVector Word8,
    prg_ram      :: VM.IOVector Word8
}

toVector :: ByteString -> IO (VM.IOVector Word8)
toVector bs = V.unsafeThaw $ V.fromList (BS.unpack bs)


data Mapper = Mapper {
    cpuRead    :: Word16 -> IO Word8
 ,  cpuWrite   :: Word16 -> Word8 -> IO ()
 ,  ppuRead    :: Word16 -> IO Word8
 ,  ppuWrite   :: Word16 -> Word8 -> IO ()
}

mappersById :: M.Map Word8 (Bool -> Cartridge -> IO Mapper)
mappersById = M.fromList [
    (0, nrom)
  ]

dummyMapper = Mapper dummyRead dummyWrite dummyRead dummyWrite
 where dummyRead = const (pure 0); dummyWrite _ _ = pure ()

assembleCartridge :: INES -> IO Cartridge
assembleCartridge INES{..} = do
  let 
    mapperId = (flags6 `shiftR` 4) .|. (flags7 .&. 0xF0)
    mirror = if flags6 `testBit` 3 then FourScreen else (toEnum . fromEnum) (flags6 `testBit` 0)
  when (mapperId `M.notMember` mappersById) . fail $ "Mapper type " ++ show mapperId ++ " is currently not supported"
  let hasChrRam = BS.length chr_rom_bs == 0  
  chr_rom <- if hasChrRam then VM.new 0x2000 else toVector chr_rom_bs
  prg_rom <- toVector prg_rom_bs
  prg_ram <- VM.new (if prg_ram_size == 0 then 0x2000 else fromIntegral prg_ram_size)
  let 
    cart = Cartridge{..}
    mapper = dummyMapper
  assembledMapper <- (mappersById M.! mapperId) hasChrRam cart
  pure cart { mapper = assembledMapper }


loadCartridge :: FilePath -> IO Cartridge
loadCartridge path = tryLoadingINES path >>= assembleCartridge

cpuReadCartridge :: Cartridge -> Word16 -> IO Word8
cpuReadCartridge cart = cpuRead (mapper cart)

cpuWriteCartridge :: Cartridge -> Word16 -> Word8 -> IO ()
cpuWriteCartridge cart = cpuWrite (mapper cart)

ppuReadCartridge cart = ppuRead (mapper cart)
ppuWriteCartridge cart = ppuWrite (mapper cart)


-- Mappers

nrom :: Bool -> Cartridge -> IO Mapper
nrom hasChrRam Cartridge{..} = pure Mapper{..}
 where 
  chr_rom_size = VM.length chr_rom
  prg_ram_size = VM.length prg_ram
  mirrored  addr = fromIntegral $ (addr - 0x8000) .&. 0x3FFF
  intact addr = fromIntegral (addr `clearBit` 15)
  prg_ram_addr addr = (fromIntegral (addr .&. 0x1FFF)) .&. (prg_ram_size-1)
  readWith :: (Word16 -> Int) -> Word16 -> IO Word8
  readWith mode addrUnsafe
    | addr <= 0x7FFF = VM.read prg_ram (prg_ram_addr addr)
    | otherwise = VM.read prg_rom (mode addr)
    where addr = addrUnsafe .&. 0xFFFF
  cpuWrite addrUnsafe val
    | addr <= 0x7FFF = VM.write prg_ram (prg_ram_addr addr) val
    | otherwise = error $ "The program tried to cpuWrite PRG_ROM at $" ++ map toUpper (showHex addr "")
    where addr = addrUnsafe .&. 0xFFFF
  cpuRead = readWith $
    case VM.length prg_rom of
      0x4000 ->  mirrored
      0x8000 ->  intact
  mkChrAddr addrUnsafe = fromIntegral addrUnsafe .&. (chr_rom_size - 1)
  ppuRead :: Word16 -> IO Word8
  ppuRead addrUnsafe = VM.read chr_rom (mkChrAddr addrUnsafe)
  ppuWrite addrUnsafe val = if hasChrRam
    then VM.write chr_rom (mkChrAddr addrUnsafe) val 
    else error "PPU attempted to write CHR_ROM"
