{-# LANGUAGE TypeFamilies, OverloadedLists #-}

module Nes.Cartridge.Mappers (
    Mapper(..),
    mappersById
) where


import           Control.Monad
import           Data.Bits
import           Data.Functor
import           Data.IORef.Unboxed
import qualified Data.Map as M
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Nes.Cartridge.Memory


mappersById :: M.Map Word8 (Cartridge -> IO Mapper)
mappersById = [
    (0, nrom),
    --(1, mmc1),
    (2, unrom),
    (3, cnrom)
  ]


nrom :: Cartridge -> IO Mapper
nrom Cartridge{..} = pure Mapper{..}
 where
  mirroringFunction = pure $ getMirroringFunction mirror
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


mmc1 :: Cartridge -> IO Mapper
mmc1 Cartridge{..} = do
  regs@[ 
      control
    , chrb0  
    , chrb1  
    , prgb
    , load
    , loadCount ] <- replicateM 6 (newIORefU (0 :: Word8))

  let
    resetControl = modifyIORefU control (.|. 0x0C)

    serialize = MapperState <$> forM regs readIORefU
    deserialize (MapperState values) = zipWithM_ writeIORefU regs values

    mirroringFunction = do
      ctrl <- readIORefU control
      return $ case ctrl .&. 0b11 of
        0 -> (.&. 0x3FF)
        1 -> \x -> x .&. 0x3FF + 0x400
        2 -> verticalMirroring
        3 -> horizontalMirroring

    resetShiftRegister = do
      writeIORefU load      0
      writeIORefU loadCount 0
    
    cpuRead addr
      | addr < 0x8000 = VUM.read prg_ram (addrInt .&. 0x1FFF)
      | otherwise = do
        ctrl <- readIORefU control
        prg  <- readIORefU prgb
        if ctrl `testBit` 3
        then do
          let prgOffset = if ctrl `testBit` 2 then 0x4000 else 0
          VUM.read prg_rom $ prgOffset + 0x4000 * (fromIntegral $ (prg .&. 0xF) `shiftR` 1) + (addrInt .&. 0x3FFF)
        else
          VUM.read prg_rom $ 0x8000 * (fromIntegral $ (prg .&. 0xF) `shiftR` 1) + (addrInt .&. 0x7FFF)
      where addrInt = fromIntegral addr
        
    cpuWrite addr val
      | addr < 0x8000 = VUM.write prg_ram (fromIntegral addr .&. 0x1FFF) val
      | otherwise = do
        if (val `testBit` 7) then
          resetShiftRegister
        else do
          lc <- readIORefU loadCount
          modifyIORefU load $ \reg -> reg .|. ((val .&. 1) `shiftL` fromIntegral lc)

          if lc == 5
          then do
            let registerIndex = fromIntegral ((addr - 0x8000) `quot` 0x2000)
            readIORefU load >>= writeIORefU (regs !! registerIndex)
            resetShiftRegister
          else 
            modifyIORefU loadCount (+1)

    ppuRead addr = 
      let addrInt = fromIntegral addr in
      if hasChrRam 
      then VUM.read chr_rom (fromIntegral addrInt .&. 0x1FFF)
      else do
        ctrl <- readIORefU control
        chr0 <- readIORefU chrb0
        if ctrl `testBit` 4
        then 
         (if addr `testBit` 12 
          then do
            chr0 <- readIORefU chrb0
            VUM.read chr_rom $ 0x1000 * fromIntegral chr0 + (addrInt .&. 0x0FFF)
          else do
            chr1 <- readIORefU chrb1
            VUM.read chr_rom $ 0x1000 * fromIntegral chr1 + (addrInt .&. 0x0FFF))
        else 
          VUM.read chr_rom $ 0x2000 * (fromIntegral chr0 `shiftR` 1) + (addrInt .&. 0x1FFF)

    ppuWrite addr val = do
      when hasChrRam (VUM.write chr_rom (fromIntegral addr .&. 0x1FFF) val)

  resetControl
  return Mapper{..}


unrom :: Cartridge -> IO Mapper
unrom Cartridge{..} = do
  control <- newIORefU (0 :: Int)
  let
    (prgBanks :: Int) = VUM.length prg_rom `quot` 0x4000

    serialize = MapperState . return . fromIntegral <$> readIORefU control
    deserialize (MapperState [x]) = writeIORefU control $ fromIntegral x
    mirroringFunction = pure $ getMirroringFunction mirror

    cpuRead addr
      | addr < 0x8000 = VUM.read prg_ram $ fromIntegral (addr - 0x6000)
      | addr < 0xC000 = do
        offset <- readIORefU control <&> (\reg -> reg * 0x4000)
        VUM.read prg_rom (offset + fromIntegral (addr - 0x8000))
      | otherwise = VUM.read prg_rom ((prgBanks-1) * 0x4000 + fromIntegral (addr - 0xC000))
        
    cpuWrite addr val
      | addr < 0x8000 = VUM.write prg_ram (fromIntegral (addr - 0x6000)) val 
      | otherwise = control `writeIORefU` (fromIntegral val `rem` prgBanks)

    ppuRead addr = VUM.read chr_rom $ fromIntegral addr
    ppuWrite addr val = 
      if hasChrRam
      then VUM.write chr_rom (fromIntegral addr) val
      else pure ()
  return Mapper{..}


cnrom :: Cartridge -> IO Mapper
cnrom Cartridge{..} = do
  control <- newIORefU (0 :: Int)
  let
    (prgBanks :: Int) = VUM.length prg_rom `quot` 0x4000

    serialize = MapperState . return . fromIntegral <$> readIORefU control
    deserialize (MapperState [x]) = writeIORefU control $ fromIntegral x
    mirroringFunction = pure $ getMirroringFunction mirror

    bank addr = case prgBanks of
      1 -> addr .&. 0x3FFF
      2 -> addr .&. 0x7FFF

    cpuRead addr = VUM.read prg_rom $ fromIntegral (bank addr)
        
    cpuWrite _ val = control `writeIORefU` (fromIntegral val .&. 0b11)

    ppuAddr addr = do
      readIORefU control <&> (\reg -> fromIntegral reg * 0x2000 + (fromIntegral addr .&. 0x1FFF))

    ppuRead  addrIn = ppuAddr addrIn >>= VUM.read chr_rom
    ppuWrite addrIn val = do
      addr <- ppuAddr addrIn
      when hasChrRam (VUM.write chr_rom addr val)

  return Mapper{..}