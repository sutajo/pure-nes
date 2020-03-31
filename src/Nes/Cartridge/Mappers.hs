{-# LANGUAGE TypeFamilies #-}

module Nes.Cartridge.Mappers (
    Mapper(..),
    mappersById,
    nrom,
    unrom
) where


import           Data.Bits
import           Data.Functor
import           Data.IORef.Unboxed
import qualified Data.Map as M
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Nes.Cartridge.Memory


mappersById :: M.Map Word8 (Cartridge -> IO Mapper)
mappersById = M.fromList [
    (0, nrom),
    (2, unrom)
  ]


nrom :: Cartridge -> IO Mapper
nrom Cartridge{..} = pure Mapper{..}
 where 
  serialize = pure $ MapperState []
  deserialize _ = pure ()
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

    serialize = MapperState . return . fromIntegral <$> readIORefU control
    deserialize (MapperState [x]) = writeIORefU control $ fromIntegral x
    
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