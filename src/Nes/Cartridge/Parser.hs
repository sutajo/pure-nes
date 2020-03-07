{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveAnyClass #-}

module Nes.Cartridge.Parser (
  Cartridge(..),
  Mirroring(..),
  Mapper(..),
  dummyMapper,
  mappersById,
  loadCartridge,
  cpuReadCartridge,
  cpuWriteCartridge,
  ppuReadCartridge,
  ppuWriteCartridge
) where

-- INES format: https://wiki.nesdev.com/w/index.php/INES
-- https://formats.kaitai.io/ines/index.html 

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Map                    as M
import           Data.IORef.Unboxed
import           Data.Store
import           GHC.Generics
import           Data.Functor
import           Data.Word
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString      as BS hiding (readFile, putStrLn, map, notElem) 
import qualified Data.ByteString.Lazy as B (readFile, toStrict)
import           Control.Monad
import           Control.Applicative()

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
  deriving (Show, Enum, Generic, Store)
    
data Cartridge = Cartridge {
    hasChrRam    :: Bool,
    mapperId     :: Word8,
    mapper       :: Mapper,
    mirror       :: Mirroring,
    chr_rom      :: VUM.IOVector Word8,
    prg_rom      :: VUM.IOVector Word8,
    prg_ram      :: VUM.IOVector Word8
} deriving (Generic)

toVector :: ByteString -> IO (VUM.IOVector Word8)
toVector bs = VU.thaw $ VU.fromList (BS.unpack bs)


data Mapper = Mapper {
    cpuRead    :: Word16 -> IO Word8
 ,  cpuWrite   :: Word16 -> Word8 -> IO ()
 ,  ppuRead    :: Word16 -> IO Word8
 ,  ppuWrite   :: Word16 -> Word8 -> IO ()
} deriving (Generic)

mappersById :: M.Map Word8 (Cartridge -> IO Mapper)
mappersById = M.fromList [
    (0, nrom),
    (2, unrom)
  ]

dummyMapper = Mapper dummyRead dummyWrite dummyRead dummyWrite
 where dummyRead = const (pure 0); dummyWrite _ _ = pure ()

assembleCartridge :: INES -> IO Cartridge
assembleCartridge INES{..} = do
  let 
    mapperId = (flags6 `shiftR` 4) .|. (flags7 .&. 0xF0)
    mirror = if flags6 `testBit` 3 then FourScreen else (toEnum . fromEnum) (flags6 `testBit` 0)
  BS.putStr "Mirroring: " >> print mirror
  when (mapperId `M.notMember` mappersById) . fail $ "Mapper type " ++ show mapperId ++ " is currently not supported"
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

cpuReadCartridge :: Cartridge -> Word16 -> IO Word8
cpuReadCartridge cart = cpuRead (mapper cart)

cpuWriteCartridge :: Cartridge -> Word16 -> Word8 -> IO ()
cpuWriteCartridge cart = cpuWrite (mapper cart)

ppuReadCartridge cart = ppuRead (mapper cart)
ppuWriteCartridge cart = ppuWrite (mapper cart)


-- Mappers

nrom :: Cartridge -> IO Mapper
nrom Cartridge{..} = pure Mapper{..}
 where 
  chr_rom_size = VUM.length chr_rom
  prg_ram_size = VUM.length prg_ram
  mirrored  addr = fromIntegral $ (addr - 0x8000) .&. 0x3FFF
  intact addr = fromIntegral (addr `clearBit` 15)
  prg_ram_addr addr = (fromIntegral (addr .&. 0x1FFF)) .&. (prg_ram_size-1)
  readWith :: (Word16 -> Int) -> Word16 -> IO Word8
  readWith mode addrUnsafe
    | addr <= 0x7FFF = VUM.read prg_ram (prg_ram_addr addr)
    | otherwise = VUM.read prg_rom (mode addr)
    where addr = addrUnsafe .&. 0xFFFF
  cpuWrite addrUnsafe val
    | addr <= 0x7FFF = VUM.write prg_ram (prg_ram_addr addr) val
    | otherwise = pure ()
    where addr = addrUnsafe .&. 0xFFFF
  cpuRead = readWith $
    case VUM.length prg_rom of
      0x4000 ->  mirrored
      0x8000 ->  intact
  mkChrAddr addrUnsafe = fromIntegral addrUnsafe .&. (chr_rom_size - 1)
  ppuRead :: Word16 -> IO Word8
  ppuRead addrUnsafe = VUM.read chr_rom (mkChrAddr addrUnsafe)
  ppuWrite addrUnsafe val = if hasChrRam
    then VUM.write chr_rom (mkChrAddr addrUnsafe) val 
    else pure () -- On NROM writing ROM is a nop. See: https://forums.nesdev.com/viewtopic.php?f=3&t=17584


unrom :: Cartridge -> IO Mapper
unrom Cartridge{..} = do
  control <- newIORefU (0 :: Int)
  let
    
    (prgBanks :: Int) = VUM.length prg_rom `quot` 0x4000

    {-
    cpuRead :: Word16 -> IO Word8
    cpuRead addr
      | addr' >= 0xC000 = do
        VUM.read prg_rom (((prgBanks-1) * 0x4000) + (addr' - 0xC000))
      | addr' >= 0x8000 = do
        prgBank1V <- readIORefU control
        VUM.read prg_rom ((prgBank1V * 0x4000) + (addr' - 0x8000))
      | addr' >= 0x6000 = VUM.read prg_ram (addr' - 0x6000)
      | otherwise = error $ "Erroneous cart read detected!"
      where addr' = fromIntegral addr

    cpuWrite :: Word16 -> Word8 -> IO ()
    cpuWrite addr v
      | addr' >= 0x8000 = writeIORefU control (fromIntegral v `rem` prgBanks)
      | addr' >= 0x6000 = VUM.write prg_ram (addr' - 0x6000) v
      | otherwise = error $ "Erroneous cart write detected!"
      where addr' = fromIntegral addr
      -}
    
    cpuRead addr
      | addr < 0x8000 = VUM.read prg_ram $ fromIntegral (addr - 0x6000)
      | addr < 0xC000 = do
        offset <- readIORefU control <&> (\reg -> reg * 0x4000)
        VUM.read prg_rom (offset + fromIntegral (addr - 0x8000))
      | otherwise = VUM.read prg_rom ((prgBanks-1) * 0x4000 + fromIntegral (addr - 0xC000))
        
    cpuWrite addr val
      | addr < 0x8000 = VUM.write prg_ram (fromIntegral (addr - 0x6000)) val 
      | otherwise = control `writeIORefU` (fromIntegral val `rem` prgBanks)

    ppuRead  addr = VUM.read chr_rom $ fromIntegral addr
    ppuWrite addr val = 
      if hasChrRam
      then VUM.write chr_rom (fromIntegral addr) val
      else pure ()
  return Mapper{..}
