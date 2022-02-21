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
import           Nes.Emulation.Registers
import Numeric (showHex, showIntAtBase)


mappersById :: M.Map Word8 (Cartridge -> IO Mapper)
mappersById = [
    (0, nrom),
    (1, mmc1),
    (2, unrom),
    (3, cnrom)
  ]


nrom :: Cartridge -> IO Mapper
nrom Cartridge{..} = pure Mapper{..}
 where
  mirroringFunction = pure $ getMirroringFunction mirror
  serialize = pure $ MapperState []
  deserialize _ = pure ()
  chr_size = VUM.length chr
  prg_ram_size = VUM.length prg_ram
  mirrored  addr = fromIntegral $ (addr - 0x8000) .&. 0x3FFF
  intact addr = fromIntegral (addr `clearBit` 15)
  prg_ram_addr addr = fromIntegral (addr .&. 0x1FFF) .&. (prg_ram_size-1)
  readWith :: (Word16 -> Int) -> Word16 -> IO Word8
  readWith mode addrUnsafe
    | addr <= 0x7FFF = VUM.unsafeRead prg_ram (prg_ram_addr addr)
    | otherwise = VUM.unsafeRead prg_rom (mode addr)
    where addr = addrUnsafe .&. 0xFFFF
  cpuWrite addrUnsafe val
    | addr <= 0x7FFF = VUM.unsafeWrite prg_ram (prg_ram_addr addr) val
    | otherwise = pure ()
    where addr = addrUnsafe .&. 0xFFFF
  cpuRead = readWith $
    case VUM.length prg_rom of
      0x4000 ->  mirrored
      0x8000 ->  intact
  mkChrAddr addrUnsafe = fromIntegral addrUnsafe .&. (chr_size - 1)
  ppuRead :: Word16 -> IO Word8
  ppuRead addrUnsafe = VUM.unsafeRead chr (mkChrAddr addrUnsafe)
  ppuWrite addrUnsafe val = if hasChrRam
    then VUM.unsafeWrite chr (mkChrAddr addrUnsafe) val
    else pure () -- On NROM writing ROM is a nop. See: https://forums.nesdev.com/viewtopic.php?f=3&t=17584


mmc1 :: Cartridge -> IO Mapper
mmc1 Cartridge{..} = do
  let (prgBanks :: Int) = VUM.length prg_rom `quot` 0x4000

  -- Initialize registers
  control       <- newIORefU 0x1C
  load          <- newIORefU 0b10000
  selectPrg16Hi <- newIORefU $ fromIntegral (prgBanks - 1)

  [   selectChr8
    , selectChr4Lo
    , selectChr4Hi
    , selectPrg32
    , selectPrg16Lo ] <- replicateM 5 (newIORefU (0 :: Word8))

  let 
    regs :: [Register8] = [
        control,
        selectChr8,
        selectChr4Lo,
        selectChr4Hi,
        selectPrg32,
        selectPrg16Lo,
        selectPrg16Hi,
        load 
      ]

    serialize = MapperState <$> forM regs readIORefU
    deserialize (MapperState values) = zipWithM_ writeIORefU regs values

    mirroringFunction = do
      ctrl <- readIORefU control
      return $ case ctrl .&. 0b11 of
        0 -> (.&. 0x3FF)
        1 -> \x -> x .&. 0x3FF + 0x400
        2 -> verticalMirroring
        3 -> horizontalMirroring

    resetLoadRegister = writeIORefU load 0b10000

    cpuRead addr
      | addr < 0x8000 = VUM.read prg_ram (addrInt .&. 0x1FFF)
      | otherwise = do
        ctrl    <- readIORefU control
        prg32   <- readIORefU selectPrg32
        prg16Lo <- readIORefU selectPrg16Lo
        prg16Hi <- readIORefU selectPrg16Hi
        let 
          _16KbPrgMode = ctrl `testBit` 3
          mappedAddr
            | not _16KbPrgMode = 0x8000 * fromIntegral prg32 .|. (addrInt .&. 0x7FFF)
            | otherwise = fromIntegral (if addr <= 0xBFFF then prg16Lo else prg16Hi) * 0x4000 + (addrInt .&. 0x3FFF)
        VUM.read prg_rom mappedAddr
      where 
        addrInt = fromIntegral addr

    cpuWrite addr val
      | addr <= 0x7FFF = VUM.write prg_ram (fromIntegral addr .&. 0x1FFF) val
      | val `testBit` 7 = do
          resetLoadRegister
          modifyIORefU control (.|. 0xC)
      | otherwise = do
          loadRegFull <- (`testBit` 0) <$> readIORefU load
          modifyIORefU load (\reg -> ((val .&. 0b1) `shiftL` 4) .|. (reg `shiftR` 1))
          ld <- readIORefU load
          when loadRegFull $ do
            let targetRegister = (addr `shiftR` 13) .&. 0b11
            ctrl <- readIORefU control
            let _4KbChrMode = ctrl `testBit` 4
            case targetRegister of
              0 -> writeIORefU control (ld .&. 0x1F)
              1 -> do
                uncurry writeIORefU $ if _4KbChrMode then (selectChr4Lo, val .&. 0x1F) else (selectChr8, val .&. 0x1E)
              2 -> when _4KbChrMode $ do
                writeIORefU selectChr4Hi (ld .&. 0x1F)
              3 -> do
                let prgSelectMode = (ctrl `shiftR` 2) .&. 0b11
                if prgSelectMode <= 1
                then 
                  writeIORefU selectPrg32 ((ld .&. 0xE) `shiftR` 1)
                else case prgSelectMode of
                  2 -> do
                    writeIORefU selectPrg16Lo 0
                    writeIORefU selectPrg16Hi (ld .&. 0xF)
                  3 -> do
                    writeIORefU selectPrg16Lo (ld .&. 0xF)
                    writeIORefU selectPrg16Hi $ fromIntegral (prgBanks-1)
            resetLoadRegister

    ppuRead addr = do
      ctrl   <- readIORefU control
      chr4Lo <- readIORefU selectChr4Lo
      chr4Hi <- readIORefU selectChr4Hi
      chr8   <- readIORefU selectChr8
      let 
        _4KbChrMode = ctrl `testBit` 4
        mappedAddr
          | _4KbChrMode = fromIntegral (if addr <= 0x0FFF then chr4Lo else chr4Hi) * 0x1000 .|. (addrInt .&. 0x0FFF)
          | otherwise   = fromIntegral chr8 * 0x2000 .|. (addrInt .&. 0x1FFF)
      VUM.read chr mappedAddr
      where
        addrInt = fromIntegral addr

    ppuWrite addr val = do
      when hasChrRam (VUM.write chr (fromIntegral addr `mod` VUM.length chr) val)

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
      | addr < 0x8000 = VUM.unsafeRead prg_ram $ fromIntegral (addr - 0x6000)
      | addr < 0xC000 = do
        offset <- readIORefU control <&> (* 0x4000)
        VUM.unsafeRead prg_rom (offset + fromIntegral (addr - 0x8000))
      | otherwise = VUM.unsafeRead prg_rom ((prgBanks-1) * 0x4000 + fromIntegral (addr - 0xC000))

    cpuWrite addr val
      | addr < 0x8000 = VUM.unsafeWrite prg_ram (fromIntegral (addr - 0x6000)) val
      | otherwise = control `writeIORefU` (fromIntegral val `rem` prgBanks)

    ppuRead addr = VUM.unsafeRead chr $ fromIntegral addr
    ppuWrite addr val =
      if hasChrRam
      then VUM.unsafeWrite chr (fromIntegral addr) val
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

    cpuRead addr = VUM.unsafeRead prg_rom $ fromIntegral (bank addr)

    cpuWrite _ val = control `writeIORefU` (fromIntegral val .&. 0b11)

    ppuAddr addr = do
      readIORefU control <&> (\reg -> fromIntegral reg * 0x2000 + (fromIntegral addr .&. 0x1FFF))

    ppuRead = ppuAddr >=> VUM.unsafeRead chr
    ppuWrite addrIn val = do
      addr <- ppuAddr addrIn
      when hasChrRam (VUM.unsafeWrite chr addr val)

  return Mapper{..}
