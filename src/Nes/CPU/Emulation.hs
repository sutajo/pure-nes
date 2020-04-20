{-# LANGUAGE ScopedTypeVariables, LambdaCase, RecordWildCards, StrictData #-}

module Nes.CPU.Emulation (
  reset,
  clock,
  fetch,
  writeReg,
  readReg,
  modifyReg,
  read,
  write,
  readNullTerminatedString,
  processInterrupts
) where

import           Prelude hiding (read, cycle, and)
import           Control.Applicative
import           Control.Monad.Extra
import           Control.Monad.Loops
import           Nes.Emulation.Monad hiding (bit)
import           Nes.CPU.Memory as CPUMEM
import qualified Nes.APU.Emulation as APU
import qualified Nes.PPU.Emulation as PPU hiding (clock)

data Penalty = None | BoundaryCross

type Opcode = Word8

data DecodedOpcode = DecodedOpcode {
  instruction   :: Emulator CPU (),
  cycles        :: Int
}

op = DecodedOpcode

toWord8 :: Bool -> Word8
toWord8 = fromIntegral . fromEnum

toWord16 :: Bool -> Word16
toWord16 = fromIntegral . fromEnum

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 word = (fromIntegral (word `shiftR` 8) , fromIntegral (word .&. 0x00FF))

mergeWord8 :: Word8 -> Word8 -> Word16
mergeWord8 low high = (fromIntegral high `shiftL` 8) .|. fromIntegral low

-- 0x0100     0x01FF
-- [  <--GROWTH--- ]
stackBase :: Word16
stackBase = 0x0100

readReg :: Prim a => (CPU -> IORefU a) -> Emulator CPU a
readReg = flip useMemory readIORefU

writeReg :: Prim a => (CPU -> IORefU a) -> a -> Emulator CPU ()
writeReg reg val = useMemory reg (flip writeIORefU val)

modifyReg :: Prim a => (CPU -> IORefU a) -> (a -> a) -> Emulator CPU ()
modifyReg reg f = readReg reg >>= writeReg reg . f

readRAM :: Word16 -> Emulator CPU Word8
readRAM = readMemory ram

writeRAM :: Word16 -> Word8 -> Emulator CPU ()
writeRAM = writeMemory ram

cpuReadCartridge :: Word16 -> Emulator CPU Word8
cpuReadCartridge = readCartridgeWithAccessor CPUMEM.cartridgeAccess

cpuWriteCartridge :: Word16 -> Word8 -> Emulator CPU ()
cpuWriteCartridge = writeCartridgeWithAccessor CPUMEM.cartridgeAccess

read :: Word16 -> Emulator CPU Word8
read addr 
  | addr <= 0x1FFF = readRAM (addr .&. 0x7FF)
  | addr <= 0x3FFF = accessPPU $ PPU.cpuReadRegister (0x2000 .|. addr .&. 7)
  | addr <= 0x4015 = pure 0 -- getApu <&> APU.read (addr .&. 0x3FFF)
  | addr <= 0x4017 = readController (fromIntegral $ addr - 0x4016)
  | otherwise = cpuReadCartridge addr


readNullTerminatedString :: Word16 -> Emulator CPU String
readNullTerminatedString addr = map (toEnum.fromEnum) <$> unfoldrM go addr
  where
    go addr = do
      byte <- read addr
      pure $ case byte of
        0x0 -> Nothing
        ___ -> Just (byte, addr + 1)

readAddress :: Word16 -> Emulator CPU Word16
readAddress addr = liftA2 mergeWord8 (read addr) (read (addr+1))

readAddressWithBug :: Word16 -> Emulator CPU Word16
readAddressWithBug addr = do
  lo <- read addr
  hi <- read (addr+1)
  let addr = mergeWord8 lo hi
  liftA2 mergeWord8 (read addr) $
    read (
      case lo of
        0xFF -> addr .&. 0xFF00
        ____ -> addr  +  1
    )

write :: Word16 -> Word8 -> Emulator CPU ()
write addr val
  | addr <= 0x1FFF = writeRAM (addr .&. 0x7FF) val
  | addr <= 0x3FFF = accessPPU $ PPU.cpuWriteRegister (0x2000 .|. addr .&. 7) val
  | addr == 0x4014 = oamDma val
  | addr <= 0x4015 = pure () --getApu >>= setApu . APU.write (addr .&. 0x3FFF) val
  | addr == 0x4016 = forM_ [0..1] (writeController val) -- writing to 0x4016 polls both controllers
  | addr == 0x4017 = pure ()
  | otherwise = cpuWriteCartridge addr val


setFlag :: Flag -> Bool -> Emulator CPU ()
setFlag flag cond = modifyReg p (setFlag' flag cond)
  where setFlag' flag val word = (if val then setBit else clearBit) word (fromEnum flag)

clearFlag :: Flag -> Emulator CPU ()
clearFlag = flip setFlag False

setOverflow :: Word8 -> Emulator CPU ()
setOverflow word = setFlag Overflow (word `testBit` 6)

setZero :: Word8 -> Emulator CPU ()
setZero word = setFlag Zero (word == 0)

setNegative :: Word8 -> Emulator CPU ()
setNegative word = setFlag Negative (word `testBit` 7)

testFlag :: Flag -> Emulator CPU Bool
testFlag flag = readReg p <&> testFlag' flag
  where testFlag' flag word = word `testBit` (fromEnum flag)

-- Hi addr1 == Hi addr2
onDifferentPage :: Word16 -> Word16 -> Bool
onDifferentPage addr1 addr2 = addr1 .&. 0xFF00 /= addr2 .&. 0xFF00

cycle :: Int -> Emulator CPU ()
cycle n = modifyReg cyc (n+)

push :: Word8 -> Emulator CPU ()
push value = do
  sp <- readReg s
  write (fromIntegral sp + stackBase) value
  modifyReg s (flip (-) 1)

pop :: Emulator CPU Word8
pop = do
  sp <- readReg s
  let sp' = sp+1
  writeReg s sp'
  read (fromIntegral sp' `setBit` 8)

pushAddress :: Word16 -> Emulator CPU ()
pushAddress addr = do
  let (high, low) = splitWord16 addr 
  push high
  push low

popAddress :: Emulator CPU Word16
popAddress = liftA2 mergeWord8 pop pop

fetch :: Emulator CPU Opcode
fetch = readReg pc >>= read


-- http://obelisk.me.uk/6502/reference.html
-- https://www.masswerk.at/6502/6502_instruction_set.html#asl
-- https://forums.nesdev.com/viewtopic.php?f=10&t=10049


adc :: Word16 -> Emulator CPU ()
adc addr = do 
  mem   <- read addr 
  acc   <- readReg a 
  carry <- toWord8 <$> testFlag Carry
  let 
    result = mem + acc + carry
    (mem16 :: Word16) = fromIntegral mem 
  writeReg a result 
  setFlag Carry ((mem16 + fromIntegral acc + fromIntegral carry) .&. 0xFF00 /= 0)
  setFlag Overflow ((complement (acc `xor` mem) .&. (acc `xor` result)) `testBit` 7)
  setZero result
  setNegative result

  
and :: Word16 -> Emulator CPU ()
and addr = do
  byte <- read addr
  modifyReg a (.&. byte) 
  accVal <- readReg a
  setZero accVal
  setNegative accVal

asl :: Emulator CPU Word8 -> (Word8 -> Emulator CPU ()) -> Emulator CPU ()
asl acquire store = do
  val <- acquire
  setFlag Carry (val `testBit` 7)
  let result = val `shiftL` 1
  setNegative result
  setZero result
  store result

aslA :: Emulator CPU ()
aslA = asl (readReg a) (writeReg a)

aslM :: Word16 -> Emulator CPU ()
aslM addr = asl (read addr) (write addr)

-- if(pred) pc = addr
jumpWhen :: Emulator CPU Bool -> Word16 -> Emulator CPU ()
jumpWhen condition relativeAddr = 
  whenM condition $ do
    pc <- readReg pc
    let absoluteAddr = pc + relativeAddr
    cycle (if absoluteAddr `onDifferentPage` pc then 2 else 1)
    jmp absoluteAddr

bcc :: Word16 -> Emulator CPU ()
bcc = jumpWhen (testFlag Carry <&> not)

bcs :: Word16 -> Emulator CPU ()
bcs = jumpWhen (testFlag Carry)

beq :: Word16 -> Emulator CPU ()
beq = jumpWhen (testFlag Zero)

bit :: Word16 -> Emulator CPU ()
bit addr = do
  byte  <- read addr
  a'    <- readReg a
  setZero (byte .&. a')
  setNegative byte
  setOverflow byte

bmi :: Word16 -> Emulator CPU ()
bmi = jumpWhen (testFlag Negative)

bne :: Word16 -> Emulator CPU ()
bne = jumpWhen (testFlag Zero <&> not)

bpl :: Word16 -> Emulator CPU ()
bpl = jumpWhen (testFlag Negative <&> not)


-- http://forums.nesdev.com/viewtopic.php?p=7365#7365
brk :: Emulator CPU ()
brk = do
  readReg pc <&> (+1) >>= pushAddress
  php
  sei
  readAddress 0xFFFE >>= jmp

bvc :: Word16 -> Emulator CPU ()
bvc = jumpWhen (testFlag Overflow <&> not)

bvs :: Word16 -> Emulator CPU ()
bvs = jumpWhen (testFlag Overflow)

clc :: Emulator CPU ()
clc = clearFlag Carry

cld :: Emulator CPU ()
cld = clearFlag DecimalMode

cli :: Emulator CPU ()
cli = clearFlag InterruptDisable

clv :: Emulator CPU ()
clv = clearFlag Overflow

cmpWords :: (Emulator CPU Word8) -> (Emulator CPU Word8) -> Emulator CPU ()
cmpWords getA getB = do
  a <- getA
  b <- getB
  let res = a - b
  setZero res
  setNegative res
  setFlag Carry (a >= b)

cmp :: Word16 -> Emulator CPU ()
cmp addr = cmpWords (readReg a) (read addr)

cpx :: Word16 -> Emulator CPU ()
cpx addr = cmpWords (readReg x) (read addr)

cpy :: Word16 -> Emulator CPU ()
cpy addr = cmpWords  (readReg y) (read addr)

change :: (Word8 -> Word8) -> Emulator CPU Word8 -> (Word8 -> Emulator CPU ()) -> Emulator CPU ()
change f r w = do
  byte  <- r
  let result = f byte
  w result
  setZero result
  setNegative result

decrement x = x-1

dec :: Word16 -> Emulator CPU ()
dec addr = change decrement (read addr) (write addr)

dex :: Emulator CPU ()
dex = change decrement (readReg x) (writeReg x)

dey :: Emulator CPU ()
dey = change decrement (readReg y) (writeReg y)

eor :: Word16 -> Emulator CPU ()
eor word = do
  acc   <- readReg a
  byte  <- read word
  let result = acc `xor` byte
  writeReg a result 
  setZero result
  setNegative result

inc :: Word16 -> Emulator CPU ()
inc addr = change (+1) (read addr) (write addr)

inx :: Emulator CPU ()
inx = change (+1) (readReg x) (writeReg x)

iny :: Emulator CPU ()
iny = change (+1) (readReg y) (writeReg y)

jmp :: Word16 -> Emulator CPU ()
jmp = writeReg pc

jsr :: Word16 -> Emulator CPU ()
jsr addr = do
  readReg pc <&> decrement >>= pushAddress
  jmp addr 

ld :: (CPU -> IORefU Word8) -> Word16 -> Emulator CPU ()
ld reg addr = do
  read addr >>= writeReg reg
  a' <- readReg reg
  setZero a'
  setNegative a'

lda :: Word16 -> Emulator CPU ()
lda = ld a

ldx :: Word16 -> Emulator CPU ()
ldx = ld x

ldy :: Word16 -> Emulator CPU ()
ldy = ld y

lsr :: Emulator CPU Word8 -> (Word8 -> Emulator CPU ()) -> Emulator CPU ()
lsr acquire store = do
  a' <- acquire
  setFlag Carry (a' `testBit` 0)
  let result = a' `shiftR` 1
  store result
  setZero result
  setNegative result

lsrA :: Emulator CPU ()
lsrA = lsr (readReg a) (writeReg a)

lsrM :: Word16 -> Emulator CPU ()
lsrM addr = lsr (read addr) (write addr)

nop :: Emulator CPU ()
nop = pure ()

ora :: Word16 -> Emulator CPU ()
ora addr = do
  byte <- read addr
  a' <- readReg a
  let result = a' .|. byte
  writeReg a result
  setZero result
  setNegative result
  
pha :: Emulator CPU ()
pha = readReg a >>= push

php :: Emulator CPU ()
php = readReg p <&> (.|. 0x30) >>= push

pla :: Emulator CPU ()
pla = do
  val <- pop
  writeReg a val
  setZero val
  setNegative val

plp :: Emulator CPU ()
plp = pop <&> (`clearBit` 4) >>= writeReg p

rol :: Emulator CPU Word8 -> (Word8 -> Emulator CPU ()) -> Emulator CPU ()
rol acquire store = do
  val <- acquire
  c   <- toWord8 <$> testFlag Carry
  let result = (val `shiftL` 1) .|. c
  setFlag Carry (val `testBit` 7) 
  setNegative result
  setZero result
  store result

rolA :: Emulator CPU ()
rolA = rol (readReg a) (writeReg a)

rolM :: Word16 -> Emulator CPU ()
rolM addr = rol (read addr) (write addr)

ror :: Emulator CPU Word8 -> (Word8 -> Emulator CPU ()) -> Emulator CPU ()
ror acquire store = do
  val <- acquire
  c   <- toWord8 <$> testFlag Carry
  let result = (val `shiftR` 1) .|. (c `shiftL` 7)
  setFlag Carry (val `testBit` 0)
  setNegative result
  setZero result
  store result

rorA :: Emulator CPU ()
rorA = ror (readReg a) (writeReg a)

rorM :: Word16 -> Emulator CPU ()
rorM addr = ror (read addr) (write addr)

rti :: Emulator CPU ()
rti = do
  pop >>= writeReg p
  popAddress >>= jmp

rts :: Emulator CPU ()
rts = do
  addr <- popAddress
  jmp (addr+1)

sbc :: Word16 -> Emulator CPU ()
sbc addr = do 
  value <- read addr
  acc   <- readReg a
  carry <- toWord16 <$> testFlag Carry
  let 
    (result :: Word16) = val16 + acc16 + carry
    acc16 = fromIntegral acc
    val16 = fromIntegral value `xor` 0x00FF
    res8 = fromIntegral result
  setFlag Carry (result .&. 0xFF00 /= 0)
  setZero res8
  setNegative res8
  setFlag Overflow ((0 /= ) $ (result `xor` acc16) .&. (result `xor` val16) .&. 0x0080)
  writeReg a res8

sec :: Emulator CPU ()
sec = setFlag Carry True

sed :: Emulator CPU ()
sed = setFlag DecimalMode True

sei :: Emulator CPU ()
sei = setFlag InterruptDisable True

st :: (CPU -> IORefU Word8) -> Word16 -> Emulator CPU ()
st reg addr = readReg reg >>= write addr

sta = st a

stx = st x

sty = st y

transfer a b = readReg a >>= writeReg b
transferAndSetFlags a b = do
  transfer a b
  val <- readReg b
  setZero val
  setNegative val 

tax :: Emulator CPU ()
tax = transferAndSetFlags a x

tay :: Emulator CPU ()
tay = transferAndSetFlags a y

tsx :: Emulator CPU ()
tsx = transferAndSetFlags s x

txa :: Emulator CPU ()
txa = transferAndSetFlags x a

txs :: Emulator CPU ()
txs = transfer x s

tya :: Emulator CPU ()
tya = transferAndSetFlags y a

-- Addressing modes

accumulator :: Emulator CPU ()
accumulator = pure ()

immediate :: Emulator CPU Word16
immediate = readReg pc <* modifyReg pc (+1)

implied :: Emulator CPU ()
implied = pure ()

zeroPage :: Emulator CPU Word16
zeroPage = (fetch <&> fromIntegral) <* modifyReg pc (+1)

zeroPageX :: Emulator CPU Word16
zeroPageX = do
  addr  <- fetch
  modifyReg pc (+1)
  xreg  <- readReg x
  return (fromIntegral $ addr + xreg)

zeroPageY :: Emulator CPU Word16
zeroPageY = do
  y     <- readReg y
  addr  <- fetch
  modifyReg pc (+1)
  return (fromIntegral $ addr + y)

absolute :: Emulator CPU Word16
absolute = (readReg pc >>= readAddress) <* modifyReg pc (+2)

addPenaltyCycles :: Bool -> Penalty -> Emulator CPU ()
addPenaltyCycles _____ None = pure ()
addPenaltyCycles False ____ = pure ()
addPenaltyCycles True BoundaryCross = cycle 1

absoluteGen :: Emulator CPU Word8 -> Penalty -> Emulator CPU Word16
absoluteGen acquire penalty = do
  addr  <- absolute 
  reg16 <- fromIntegral <$> acquire
  let 
    result = addr + reg16
    pageCrossed = addr `onDifferentPage` result
  addPenaltyCycles pageCrossed penalty
  return result

absoluteX :: Emulator CPU Word16
absoluteX = absoluteGen (readReg x) None

absoluteXP :: Emulator CPU Word16
absoluteXP = absoluteGen (readReg x) BoundaryCross

absoluteY :: Emulator CPU Word16
absoluteY = absoluteGen (readReg y) None

absoluteYP :: Emulator CPU Word16
absoluteYP = absoluteGen (readReg y) BoundaryCross

indirect :: Emulator CPU Word16
indirect = (readReg pc >>= readAddressWithBug) <* modifyReg pc (+2)

readZeroPageAddress :: Word16 -> Emulator CPU Word16
readZeroPageAddress x = liftA2 mergeWord8 (read x) $ read ((x+1) .&. 0xFF)

indirectX :: Emulator CPU Word16
indirectX = do
  addr <- fetch
  modifyReg pc (+1)
  x <- readReg x
  let abs = addr + x
  readZeroPageAddress $ fromIntegral abs

indirectYGen :: Penalty -> Emulator CPU Word16
indirectYGen penalty = do
  yreg  <- readReg y
  addr  <- fetch >>= readZeroPageAddress . fromIntegral
  modifyReg pc (+1)
  let result = addr + fromIntegral yreg
  addPenaltyCycles (addr `onDifferentPage` result) penalty
  return result

indirectY :: Emulator CPU Word16
indirectY = indirectYGen None

indirectYP :: Emulator CPU Word16
indirectYP = indirectYGen BoundaryCross

relative :: Emulator CPU Word16
relative = do
  addr_relative <- fetch <&> fromIntegral
  modifyReg pc (+1)
  return $ 
    if addr_relative `testBit` 7
    then addr_relative .|. 0xFF00
    else addr_relative

decodeOpcode :: Opcode -> DecodedOpcode
decodeOpcode opcode = case opcode of
  0x00 -> op (implied >>  brk)      7;  0x80 -> op (immediate >> nop)     2;
  0x01 -> op (indirectX >>= ora)    6;  0x81 -> op (indirectX >>= sta)    6; 
  0x02 -> op (implied >> kil)       0;  0x82 -> op (immediate >> nop)     2; 
  0x03 -> op (indirectX >>= slo)    8;  0x83 -> op (indirectX >>= sax)    6; 
  0x04 -> op (zeroPage >>  nop)     3;  0x84 -> op (zeroPage >>= sty)     3; 
  0x05 -> op (zeroPage >>= ora)     3;  0x85 -> op (zeroPage >>= sta)     3; 
  0x06 -> op (zeroPage >>= aslM)    5;  0x86 -> op (zeroPage >>= stx)     3;
  0x07 -> op (zeroPage >>= slo)     5;  0x87 -> op (zeroPage >>= sax)     3; 
  0x08 -> op (implied >>  php)      3;  0x88 -> op (implied >> dey)       2; 
  0x09 -> op (immediate >>= ora)    2;  0x89 -> op (immediate >> nop)     2; 
  0x0A -> op (accumulator >> aslA)  2;  0x8A -> op (implied >> txa)       2; 
  0x0B -> op (immediate >>= anc)    2;  0x8B -> op (immediate >>= xaa)    2; 
  0x0C -> op (absolute >>  nop)     4;  0x8C -> op (absolute >>= sty)     4;
  0x0D -> op (absolute >>= ora)     4;  0x8D -> op (absolute >>= sta)     4;
  0x0E -> op (absolute >>= aslM)    6;  0x8E -> op (absolute >>= stx)     4; 
  0x0F -> op (absolute >>= slo)     6;  0x8F -> op (absolute >>= sax)     4;
  0x10 -> op (relative >>= bpl)     2;  0x90 -> op (relative  >>= bcc)    2;
  0x11 -> op (indirectYP >>= ora)   5;  0x91 -> op (indirectY >>= sta)    6;
  0x12 -> op (implied >> kil)       0;  0x92 -> op (implied   >>  kil)    0; 
  0x13 -> op (indirectY >>= slo)    8;  0x93 -> op (indirectY >>= ahx)    6;
  0x14 -> op (zeroPageX >>  nop)    4;  0x94 -> op (zeroPageX >>= sty)    4; 
  0x15 -> op (zeroPageX >>= ora)    4;  0x95 -> op (zeroPageX >>= sta)    4; 
  0x16 -> op (zeroPageX >>= aslM)   6;  0x96 -> op (zeroPageY >>= stx)    4; 
  0x17 -> op (zeroPageX >>= slo)    6;  0x97 -> op (zeroPageY >>= sax)    4; 
  0x18 -> op (implied >> clc)       2;  0x98 -> op (implied   >> tya)     2;
  0x19 -> op (absoluteYP >>= ora)   4;  0x99 -> op (absoluteY >>= sta)    5; 
  0x1A -> op (implied >> nop)       2;  0x9A -> op (implied   >> txs)     2;
  0x1B -> op (absoluteY  >>= slo)   7;  0x9B -> op (absoluteY >>= tas)    5;
  0x1C -> op (absoluteXP >>  nop)   4;  0x9C -> op (absoluteX >>= shy)    5; 
  0x1D -> op (absoluteXP >>= ora)   4;  0x9D -> op (absoluteX >>= sta)    5; 
  0x1E -> op (absoluteX >>= aslM)   7;  0x9E -> op (absoluteY >>= shx)    5;
  0x1F -> op (absoluteX >>= slo)    7;  0x9F -> op (absoluteY >>= ahx)    5;
  0x20 -> op (absolute >>= jsr)     6;  0xA0 -> op (immediate >>= ldy)    2;
  0x21 -> op (indirectX >>= and)    6;  0xA1 -> op (indirectX >>= lda)    6;
  0x22 -> op (implied >> kil)       0;  0xA2 -> op (immediate >>= ldx)    2;
  0x23 -> op (indirectX >>= rla)    8;  0xA3 -> op (indirectX >>= lax)    6;
  0x24 -> op (zeroPage >>= bit)     3;  0xA4 -> op (zeroPage >>= ldy)     3;
  0x25 -> op (zeroPage >>= and)     3;  0xA5 -> op (zeroPage >>= lda)     3;
  0x26 -> op (zeroPage >>= rolM)    5;  0xA6 -> op (zeroPage >>= ldx)     3;
  0x27 -> op (zeroPage >>= rla)     5;  0xA7 -> op (zeroPage >>= lax)     3;
  0x28 -> op (implied >> plp)       4;  0xA8 -> op (implied  >> tay)      2;
  0x29 -> op (immediate >>= and)    2;  0xA9 -> op (immediate >>= lda)    2;
  0x2A -> op (accumulator >> rolA)  2;  0xAA -> op (implied   >> tax)     2;
  0x2B -> op (immediate >>= anc)    2;  0xAB -> op (immediate >>= lax)    2;
  0x2C -> op (absolute >>= bit)     4;  0xAC -> op (absolute >>= ldy)     4; 
  0x2D -> op (absolute >>= and)     4;  0xAD -> op (absolute >>= lda)     4; 
  0x2E -> op (absolute >>= rolM)    6;  0xAE -> op (absolute >>= ldx)     4; 
  0x2F -> op (absolute >>= rla)     6;  0xAF -> op (absolute >>= lax)     4; 
  0x30 -> op (relative >>= bmi)     2;  0xB0 -> op (relative >>= bcs)     2; 
  0x31 -> op (indirectYP >>= and)   5;  0xB1 -> op (indirectYP >>= lda)   5;
  0x32 -> op (implied >> kil)       0;  0xB2 -> op (implied >> kil)       0; 
  0x33 -> op (indirectY >>= rla)    8;  0xB3 -> op (indirectYP >>= lax)   5;
  0x34 -> op (zeroPageX >>  nop)    4;  0xB4 -> op (zeroPageX >>= ldy)    4;
  0x35 -> op (zeroPageX >>= and)    4;  0xB5 -> op (zeroPageX >>= lda)    4;
  0x36 -> op (zeroPageX >>= rolM)   6;  0xB6 -> op (zeroPageY >>= ldx)    4;
  0x37 -> op (zeroPageX >>= rla)    6;  0xB7 -> op (zeroPageY >>= lax)    4;
  0x38 -> op (implied >> sec)       2;  0xB8 -> op (implied >> clv)       2;
  0x39 -> op (absoluteYP >>= and)   4;  0xB9 -> op (absoluteYP >>= lda)   4;
  0x3A -> op (implied >> nop)       2;  0xBA -> op (implied >> tsx)       2;
  0x3B -> op (absoluteY >>=  rla)   7;  0xBB -> op (absoluteY >>= las)    4;
  0x3C -> op (absoluteXP >>  nop)   4;  0xBC -> op (absoluteXP >>= ldy)   4;
  0x3D -> op (absoluteXP >>= and)   4;  0xBD -> op (absoluteXP >>= lda)   4;
  0x3E -> op (absoluteX >>= rolM)   7;  0xBE -> op (absoluteYP >>= ldx)   4; 
  0x3F -> op (absoluteX >>= rla)    7;  0xBF -> op (absoluteY >>= lax)    4; 
  0x40 -> op (implied    >> rti)    6;  0xC0 -> op (immediate >>= cpy)    2;
  0x41 -> op (indirectX  >>= eor)   6;  0xC1 -> op (indirectX >>= cmp)    6;
  0x42 -> op (implied    >> kil)    0;  0xC2 -> op (immediate >>  nop)    2;
  0x43 -> op (indirectX  >>= sre)   8;  0xC3 -> op (indirectX >>= dcp)    8;
  0x44 -> op (zeroPage >>  nop)     3;  0xC4 -> op (zeroPage >>= cpy)     3;
  0x45 -> op (zeroPage >>= eor)     3;  0xC5 -> op (zeroPage >>= cmp)     3;
  0x46 -> op (zeroPage >>= lsrM)    5;  0xC6 -> op (zeroPage >>= dec)     5;
  0x47 -> op (zeroPage >>= sre)     5;  0xC7 -> op (zeroPage >>= dcp)     5;
  0x48 -> op (implied >>  pha)      3;  0xC8 -> op (implied >> iny)       2;
  0x49 -> op (immediate >>= eor)    2;  0xC9 -> op (immediate >>= cmp)    2;
  0x4A -> op (accumulator >> lsrA)  2;  0xCA -> op (implied   >> dex)     2;
  0x4B -> op (immediate >>= alr)    2;  0xCB -> op (immediate >>= axs)    2;
  0x4C -> op (absolute >>= jmp)     3;  0xCC -> op (absolute  >>= cpy)    4;
  0x4D -> op (absolute >>= eor)     4;  0xCD -> op (absolute  >>= cmp)    4;
  0x4E -> op (absolute >>= lsrM)    6;  0xCE -> op (absolute  >>= dec)    6;
  0x4F -> op (absolute >>= sre)     6;  0xCF -> op (absolute  >>= dcp)    6; 
  0x50 -> op (relative >>= bvc)     2;  0xD0 -> op (relative  >>= bne)    2;
  0x51 -> op (indirectYP >>= eor)   5;  0xD1 -> op (indirectYP >>= cmp)   5;
  0x52 -> op (implied >> kil)       0;  0xD2 -> op (implied >>  kil)      0;
  0x53 -> op (indirectY >>= sre)    8;  0xD3 -> op (indirectY >>= dcp)    8;
  0x54 -> op (zeroPageX  >>  nop)   4;  0xD4 -> op (zeroPageX >>  nop)    4;
  0x55 -> op (zeroPageX  >>= eor)   4;  0xD5 -> op (zeroPageX >>= cmp)    4;
  0x56 -> op (zeroPageX  >>= lsrM)  6;  0xD6 -> op (zeroPageX >>= dec)    6;
  0x57 -> op (zeroPageX  >>= sre)   6;  0xD7 -> op (zeroPageX >>= dcp)    6;
  0x58 -> op (implied >> cli)       2;  0xD8 -> op (implied >> cld)       2;
  0x59 -> op (absoluteYP >>= eor)   4;  0xD9 -> op (absoluteYP >>= cmp)   4;
  0x5A -> op (implied >> nop)       2;  0xDA -> op (implied >> nop)       2;
  0x5B -> op (absoluteY >>= sre)    7;  0xDB -> op (absoluteY >>= dcp)    7;
  0x5C -> op (absoluteXP >> nop)    4;  0xDC -> op (absoluteXP >> nop)    4;
  0x5D -> op (absoluteXP >>= eor)   4;  0xDD -> op (absoluteXP >>= cmp)   4;
  0x5E -> op (absoluteX >>= lsrM)   7;  0xDE -> op (absoluteX >>= dec)    7;
  0x5F -> op (absoluteX >>= sre)    7;  0xDF -> op (absoluteX >>= dcp)    7;
  0x60 -> op (implied >> rts)       6;  0xE0 -> op (immediate >>= cpx)    2;
  0x61 -> op (indirectX >>= adc)    6;  0xE1 -> op (indirectX >>= sbc)    6;
  0x62 -> op (implied >> kil)       0;  0xE2 -> op (immediate >> nop)     2;
  0x63 -> op (indirectX >>= rra)    8;  0xE3 -> op (indirectX >>= isc)    8;
  0x64 -> op (zeroPage >>  nop)     3;  0xE4 -> op (zeroPage >>= cpx)     3;
  0x65 -> op (zeroPage >>= adc)     3;  0xE5 -> op (zeroPage >>= sbc)     3;
  0x66 -> op (zeroPage >>= rorM)    5;  0xE6 -> op (zeroPage >>= inc)     5;
  0x67 -> op (zeroPage >>= rra)     5;  0xE7 -> op (zeroPage >>= isc)     5;
  0x68 -> op (implied >> pla)       4;  0xE8 -> op (implied >>   inx)     2;
  0x69 -> op (immediate >>= adc)    2;  0xE9 -> op (immediate >>= sbc)    2;
  0x6A -> op (accumulator >> rorA)  2;  0xEA -> op (implied >> nop)       2;
  0x6B -> op (immediate >>= arr)    2;  0xEB -> op (immediate >>= sbc)    2;
  0x6C -> op (indirect >>= jmp)     5;  0xEC -> op (absolute >>= cpx)     4;
  0x6D -> op (absolute >>= adc)     4;  0xED -> op (absolute >>= sbc)     4; 
  0x6E -> op (absolute >>= rorM)    6;  0xEE -> op (absolute >>= inc)     6;
  0x6F -> op (absolute >>= rra)     6;  0xEF -> op (absolute >>= isc)     6;
  0x70 -> op (relative >>= bvs)     2;  0xF0 -> op (relative >>= beq)     2;
  0x71 -> op (indirectYP >>= adc)   5;  0xF1 -> op (indirectYP >>= sbc)   5;
  0x72 -> op (implied >> kil)       0;  0xF2 -> op (implied >>  kil)      0;
  0x73 -> op (indirectY >>= rra)    8;  0xF3 -> op (indirectY >>= isc)    8;
  0x74 -> op (zeroPageX >>  nop)    4;  0xF4 -> op (zeroPageX >> nop)     4;
  0x75 -> op (zeroPageX >>= adc)    4;  0xF5 -> op (zeroPageX >>= sbc)    4; 
  0x76 -> op (zeroPageX >>= rorM)   6;  0xF6 -> op (zeroPageX >>= inc)    6;
  0x77 -> op (zeroPageX >>= rra)    6;  0xF7 -> op (zeroPageX >>= isc)    6;
  0x78 -> op (implied >> sei)       2;  0xF8 -> op (implied >> sed)       2;
  0x79 -> op (absoluteYP >>= adc)   4;  0xF9 -> op (absoluteYP >>= sbc)   4;
  0x7A -> op (implied >> nop)       2;  0xFA -> op (implied >> nop)       2;
  0x7B -> op (absoluteY >>= rra)    7;  0xFB -> op (absoluteY >>= isc)    7;
  0x7C -> op (absoluteXP >> nop)    4;  0xFC -> op (absoluteXP >> nop)    4;
  0x7D -> op (absoluteXP >>= adc)   4;  0xFD -> op (absoluteXP >>= sbc)   4;
  0x7E -> op (absoluteX >>= rorM)   7;  0xFE -> op (absoluteX >>= inc)    7;
  0x7F -> op (absoluteX >>= rra)    7;  0xFF -> op (absoluteX >>= isc)    7;

-- http://www.ffd2.com/fridge/docs/6502-NMOS.extra.opcodes
-- Unofficial instructions:

andThen = liftA2 (>>)

ahx _ = pure ()

alr addr = and addr >> lsrA

anc addr = do
  a' <- readReg a
  v' <- read addr
  let result = a' .&. v'
  writeReg a result
  setZero result
  setNegative result
  testFlag Negative >>= setFlag Carry

arr addr = do
  and addr
  rorA
  a <- readReg a
  let bit6 = a `testBit` 6
  setFlag Overflow (bit6 `xor` (a `testBit` 5))
  setFlag Carry bit6

axs addr = do
  a'    <- readReg a
  x'    <- readReg x
  byte  <- read addr
  let 
    result = a' .&. x'
    diff = result - byte
  writeReg x diff
  setFlag Carry (byte <= result)
  setZero diff
  setNegative diff

dcp = dec `andThen` cmp

isc = inc `andThen` sbc

kil = error "KIL instruction executed"

las addr = do
  byte <- read addr
  sp   <- readReg s
  let result = byte .&. sp
  forM_ [a,s,x] $ (`writeReg` result)
  setZero result
  setNegative result

lax addr = do
  byte <- read addr
  writeReg x byte
  writeReg a byte
  setZero byte
  setNegative byte

rla = rolM `andThen` and

rra = rorM `andThen` adc

sax addr = liftA2 (.&.) (readReg x) (readReg a) >>= write addr

-- http://forums.nesdev.com/viewtopic.php?f=3&t=3831&start=30
sh reg addr = do
  let (_, lo) = splitWord16 addr
  reg <- readReg reg 
  let 
    result     = (fromIntegral (addr `shiftR` 8) + 1) .&. reg
    targetAddr = mergeWord8 lo result
  write targetAddr result

shx = sh x

shy = sh y

slo = aslM `andThen` ora

sre = lsrM `andThen` eor

tas _ = pure ()

xaa _ = pure ()


elapsedCycles :: Emulator CPU a -> Emulator CPU Int
elapsedCycles operation = do
  cyclesBefore <- readReg cyc
  operation
  readReg cyc <&> (\cyclesAfter -> cyclesAfter - cyclesBefore)


nmi :: Emulator CPU ()
nmi = do
  readReg pc >>= pushAddress
  setFlag InterruptDisable True
  readReg p <&> (`clearBit` 4) >>= push
  readAddress 0xFFFA >>= jmp
  cycle 8


irq :: Emulator CPU ()
irq = do
  irqEnabled <- not <$> testFlag InterruptDisable
  when irqEnabled $ do
    readReg pc >>= pushAddress
    setFlag InterruptDisable True
    readReg p <&> (`clearBit` 4) >>= push
    readAddress 0xFFFE >>= jmp
    cycle 7


tickInterruptTimer :: (InterruptAccess -> Register8) -> Emulator CPU () -> Emulator CPU ()
tickInterruptTimer timer interrupt = do
  remainingClocks <- readReg (timer . interrupts)

  when (remainingClocks == 1) $ do
    interrupt

  when (remainingClocks > 0) $ do
    modifyReg (timer . interrupts) decrement



processInterrupts = do
  tickInterruptTimer CPUMEM.nmi Nes.CPU.Emulation.nmi
  tickInterruptTimer CPUMEM.irq Nes.CPU.Emulation.irq
  setFlag Unused True


oamDma :: Word8 -> Emulator CPU ()
oamDma pageId = do
  let
    (baseAddr :: Word16) = fromIntegral pageId `shiftL` 8

  (oamOffset :: Word16) <- directPPUAccess $ PPU.getOamAddr <&> fromIntegral

  let 
    copyByte source dest = do
      byte <- read source
      directPPUAccess $ PPU.writeOam (fromIntegral dest) byte

  zipWithM_ copyByte [baseAddr..baseAddr+0xFF] [oamOffset..oamOffset+0xFF]
  cycles <- readReg cyc <&> fromIntegral
  cycle (513 + cycles .&. 0x1)



-- https://forums.nesdev.com/viewtopic.php?f=3&t=14231
reset :: Emulator CPU ()
reset = do
  readAddress 0xFFFC >>= jmp
  writeReg p 0x34
  writeReg s 0xFD
  cycle 7


runInstruction :: Emulator CPU () -> Int -> Emulator CPU ()
runInstruction instruction cycles = do
  modifyReg pc (+1)
  instruction
  cycle cycles
  setFlag Unused True


clock :: Emulator CPU Int
clock = do
  DecodedOpcode{instruction, cycles} <- do
    fetch <&> decodeOpcode

  elapsedCycles $ runInstruction instruction cycles