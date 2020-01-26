{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}

module Nes.CPUEmulator(
  initialize,
  reset,
  clock
)where

import Prelude hiding (read, cycle, and)
import Control.Monad.Reader
import Data.IORef.Unboxed (IORefU, readIORefU, writeIORefU)
import Data.Array.IO
import Data.Primitive(Prim)
import Data.Word
import Data.Bits hiding (bit)
import Data.Functor
import Nes.EmulatorMonad
import Nes.CPU6502

data Penalty = None | BoundaryCross deriving (Enum)

data Opcode = Opcode {
  instruction   :: Emulator (),
  length        :: !Word8,
  cycles        :: !Int
}

op = Opcode

initialize :: IO CPU
initialize = undefined

reset :: Emulator ()
reset = undefined

toWord8 :: Bool -> Word8
toWord8 = fromIntegral . fromEnum

toWord16 :: Bool -> Word16
toWord16 = fromIntegral . fromEnum

word16toWord8 :: Word16 -> (Word8, Word8)
word16toWord8 word = (fromIntegral ((word `shiftR` 8) .&. 0xFF) , fromIntegral (word .&. 0x00FF))

word8toWord16 :: Word8 -> Word8 -> Word16
word8toWord16 low high = (fromIntegral high `shiftL` 8) .|. fromIntegral low

-- 0x0100 - 0x01FF
-- [  <--GROW--- ]
stackBase :: Word16
stackBase = 0x0100

useCpu :: Prim a =>  (IORefU a -> IO b) -> (CPU -> IORefU a) -> Emulator b
useCpu action field = useMemory (field . cpu) action

readReg :: Prim a => (CPU -> IORefU a) -> Emulator a
readReg = useCpu readIORefU

writeReg :: Prim a => (CPU -> IORefU a) -> a -> Emulator ()
writeReg reg val = useCpu (flip writeIORefU val) reg

modifyReg :: Prim a => (CPU -> IORefU a) -> (a -> a) -> Emulator ()
modifyReg reg f = readReg reg >>= writeReg reg . f

readRAM :: Word16 -> Emulator Word8
readRAM addr = useMemory ram $ (flip readArray addr)

writeRAM :: Word16 -> Word8 -> Emulator ()
writeRAM addr val = useMemory ram $ (\arr -> writeArray arr addr val)

readIORegisters :: Word16 -> Emulator Word8
readIORegisters addr = useMemory ioRegisters $ (flip readArray addr)

writeIORegisters :: Word16 -> Word8 -> Emulator ()
writeIORegisters addr val = useMemory ioRegisters $ (\arr -> writeArray arr addr val)

zeropage :: Word16 -> Word8 -> Word16
zeropage arg reg = (arg + fromIntegral reg) `rem` 0x100

read :: Word16 -> Emulator Word8
read addr 
  | addr <= 0x1FFF = readRAM (addr `rem` 0x800)         -- mirrored
  | addr <= 0x2007 = error "PPU read" -- readPPU addr
  | addr <= 0x3FFF = error "PPU read" -- readPPU (0x2000 + addr `rem` 0x8)  -- mirrored
  | addr <= 0x4017 = readIORegisters addr
  | addr <= 0xFFFF = error "Cartridge read" -- readCartridge

readAddress :: Word16 -> Emulator Word16
readAddress addr = word8toWord16 <$> read addr <*> read (addr+1)

readAddressWithBug :: Word16 -> Emulator Word16
readAddressWithBug addr = do
  lowByte <- read addr
  let (addr8 :: Word8) = fromIntegral addr 
  highByte <- read ((0xFF00 .&. addr) .|. fromIntegral (addr8 + 1))
  return (word8toWord16 lowByte highByte)

write :: Word16 -> Word8 -> Emulator ()
write addr val
  | addr <= 0x1FFF = writeRAM addr val
  | addr <= 0x2007 = error "PPU write"
  | addr <= 0x3FFF = error "PPU write"
  | addr <= 0x4017 = writeIORegisters addr val
  | addr <= 0xFFFF = error "Cartridge write"

setFlag :: Flag -> Bool -> Emulator ()
setFlag flag cond = modifyReg p (setFlag' flag cond)
  where setFlag' flag val word = (if val then setBit else clearBit) word (fromEnum flag)

clearFlag :: Flag -> Emulator ()
clearFlag = flip setFlag False

setOverflow :: Word8 -> Emulator ()
setOverflow word = setFlag Overflow (word `testBit` 6)

setZero :: Word8 -> Emulator ()
setZero word = setFlag Zero (word == 0)

setNegative :: Word8 -> Emulator ()
setNegative word = setFlag Negative (word `testBit` 7)

testFlag :: Flag -> Emulator Bool
testFlag flag = readReg p <&> testFlag' flag
  where testFlag' flag word = word `testBit` (fromEnum flag)

-- Hi addr1 == Hi add2
onDifferentPage :: Word16 -> Word16 -> Bool
onDifferentPage addr1 addr2 = addr1 .&. 0xFF00 /= addr2 .&. 0xFF00

cycle :: Int -> Emulator ()
cycle n = modifyReg cyc (n+)

push :: Word8 -> Emulator ()
push value = do
  sp <- readReg s
  write (fromIntegral sp + stackBase) value
  modifyReg s (flip (-) 1)

pop :: Emulator Word8
pop = do
  sp <- readReg s
  let newSp :: Word8 = sp+1
  writeReg s newSp
  read (0x100 + fromIntegral newSp)

pushAddress :: Word16 -> Emulator ()
pushAddress addr = do
  let (high, low) = word16toWord8 addr 
  push high
  push low

popAddress :: Emulator Word16
popAddress = word8toWord16 <$> pop <*> pop

fetch :: Emulator Word16
fetch = readReg pc

fetchNext = fetchNext 


-- http://obelisk.me.uk/6502/reference.html
-- https://www.masswerk.at/6502/6502_instruction_set.html#asl


adc :: Word16 -> Emulator ()
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

  
and :: Word16 -> Emulator ()
and addr = do
  byte <- read addr
  modifyReg a (.&. byte) 
  accVal <- readReg a
  setZero accVal
  setNegative accVal

aslGeneral :: Emulator Word8 -> (Word8 -> Emulator ()) -> Emulator ()
aslGeneral acquire store = do
  val <- acquire
  setFlag Carry (val `testBit` 7)
  let result = val `shiftL` 1
  setNegative result
  setZero result
  store result

aslA :: Emulator ()
aslA = aslGeneral (readReg a) (writeReg a)

aslM :: Word16 -> Emulator ()
aslM addr = aslGeneral (read addr) (write addr)

-- if(pred) pc = addr
jumpWhen :: Word16 -> Bool -> Emulator ()
jumpWhen addr pred = when pred $ do
  pc' <- fetch
  let newPc = pc' + addr
  cycle (if newPc `onDifferentPage` pc' then 2 else 1)
  writeReg pc newPc

bcc :: Word16 -> Emulator ()
bcc addr = testFlag Carry >>= jumpWhen addr . not

bcs :: Word16 -> Emulator ()
bcs addr = testFlag Carry >>= jumpWhen addr

beq :: Word16 -> Emulator ()
beq addr = testFlag Zero >>= jumpWhen addr

bit :: Word16 -> Emulator ()
bit addr = do
  byte  <- read addr
  a'    <- readReg a
  setZero (byte .&. a')
  setNegative byte
  setOverflow byte

bmi :: Word16 -> Emulator ()
bmi addr = testFlag Negative >>= jumpWhen addr

bne :: Word16 -> Emulator ()
bne addr = testFlag Zero >>= jumpWhen addr . not

bpl :: Word16 -> Emulator ()
bpl addr = testFlag Negative >>= jumpWhen addr . not

brk :: Word16 -> Emulator ()
brk addr = do
  sei
  fetchNext >>= pushAddress
  php 
  readAddress 0xFFFE >>= writeReg pc

bvc :: Word16 -> Emulator ()
bvc addr = testFlag Overflow >>= jumpWhen addr . not

bvs :: Word16 -> Emulator ()
bvs addr = testFlag Overflow >>= jumpWhen addr

clc :: Emulator ()
clc = clearFlag Carry

cld :: Emulator ()
cld = clearFlag DecimalMode

cli :: Emulator ()
cli = clearFlag InterruptDisable

clv :: Emulator ()
clv = clearFlag Overflow

cmpWords :: (Emulator Word8) -> (Emulator Word8) -> Emulator ()
cmpWords a' b' = do
  a <- a'
  b <- b' 
  let res = a - b
  setZero res
  setNegative res
  setFlag Carry (a >= b)

cmp :: Word16 -> Emulator ()
cmp addr = cmpWords (readReg a) (read addr)

cpx :: Word16 -> Emulator ()
cpx addr = cmpWords (readReg x) (read addr)

cpy :: Word16 -> Emulator ()
cpy addr = cmpWords  (readReg y) (read addr)

change :: (Word8 -> Word8) -> Emulator Word8 -> (Word8 -> Emulator ()) -> Emulator ()
change f r w  = do
  byte  <- r
  let result = f byte
  w result
  setZero result
  setNegative result

decrement x = x-1

dec :: Word16 -> Emulator ()
dec addr = change decrement (read addr) (write addr)  

dex :: Emulator ()
dex = change decrement (readReg x) (writeReg x)

dey :: Emulator ()
dey = change decrement (readReg y) (writeReg y)

eor :: Word16 -> Emulator ()
eor word = do
  acc   <- readReg a
  byte  <- read word
  let result = acc `xor` byte
  writeReg a result 
  setZero result
  setNegative result

inc :: Word16 -> Emulator ()
inc addr = change (1+) (read addr) (write addr)

inx :: Emulator ()
inx = change (1+) (readReg x) (writeReg x)

iny :: Emulator ()
iny = change (1+) (readReg y) (writeReg y)

jmp :: Word16 -> Emulator ()
jmp addr = writeReg pc addr

jsr :: Word16 -> Emulator ()
jsr addr = do
  fetch <&> decrement >>= pushAddress
  jmp addr 

ld :: (CPU -> IORefU Word8) -> Word16 -> Emulator ()
ld reg addr = do
  read addr >>= writeReg reg
  a' <- readReg reg
  setZero a'
  setNegative a'

lda :: Word16 -> Emulator ()
lda = ld a

ldx :: Word16 -> Emulator ()
ldx = ld x

ldy :: Word16 -> Emulator ()
ldy = ld y

lsrGeneral :: Emulator Word8 -> (Word8 -> Emulator ()) -> Emulator ()
lsrGeneral acquire store = do
  a' <- acquire
  setFlag Carry (a' `testBit` 0)
  let result = a' `shiftR` 1
  store result
  setZero result
  setNegative result

lsrA :: Emulator ()
lsrA = lsrGeneral (readReg a) (writeReg a)

lsrM :: Word16 -> Emulator ()
lsrM addr = lsrGeneral (read addr) (write addr)

nop :: Emulator ()
nop = pure ()

ora :: Word16 -> Emulator ()
ora addr = do
  byte <- read addr
  a' <- readReg a
  let result = a' .|. byte
  writeReg a result
  setZero result
  setNegative result
  
pha :: Emulator ()
pha = readReg a >>= push

php :: Emulator ()
php = readReg p >>= push

pla :: Emulator ()
pla = do
  val <- pop
  writeReg a val
  setZero val
  setNegative val

plp :: Emulator ()
plp = pop >>= writeReg a

rolGeneral :: Emulator Word8 -> (Word8 -> Emulator ()) -> Emulator ()
rolGeneral acquire store = do
  val <- acquire
  let result = val `shiftL` 1
  setFlag Carry (val `testBit` 7) 
  c <- toWord8 <$> testFlag Carry
  store (result .|. c)

rolA :: Emulator ()
rolA = rolGeneral (readReg a) (writeReg a)

rolM :: Word16 -> Emulator ()
rolM addr = rolGeneral (read addr) (write addr)

rorGeneral :: Emulator Word8 -> (Word8 -> Emulator ()) -> Emulator ()
rorGeneral acquire store = do
  val <- acquire
  let result = val `shiftR` 1
  setFlag Carry (val `testBit` 0)
  c <- toWord8 <$> testFlag Carry
  store (result .|. (c `shiftL` 7))

rorA :: Emulator ()
rorA = rorGeneral (readReg a) (writeReg a)

rorM :: Word16 -> Emulator ()
rorM addr = rolGeneral (read addr) (write addr)

rti :: Emulator ()
rti = do
  pop >>= writeReg p
  popAddress >>= writeReg pc

rts :: Emulator ()
rts = do
  addr <- popAddress
  writeReg pc (addr+1)

sbc :: Word16 -> Emulator ()
sbc addr = do 
  value <- read addr 
  acc   <- readReg a
  carry <- toWord16 <$> testFlag Carry
  let 
    (result :: Word16) = val16 + acc16 + carry
    acc16 = fromIntegral acc
    val16 = fromIntegral value
  setFlag Carry ((result `shiftR` 8) /= 0)
  setZero (fromIntegral $ result .&. 0x00FF)
  setNegative $ fromIntegral result
  setFlag Overflow ((0 /= ) $ (result `xor` acc16) .&. (result `xor` val16) .&. 0x0080)
  writeReg a $ fromIntegral result


sei :: Emulator ()
sei = setFlag InterruptDisable True

st :: (CPU -> IORefU Word8) -> Word16 -> Emulator ()
st reg addr = readReg reg >>= write addr

sta = st a

stx = st x

sty = st y

transfer a b = readReg a >>= writeReg b

tax :: Emulator ()
tax = transfer a x

tay :: Emulator ()
tay = transfer a y

tsx :: Emulator ()
tsx = transfer s x

txa :: Emulator ()
txa = transfer x a

txs :: Emulator ()
txs = transfer x s

tya :: Emulator ()
tya = transfer y a

-- Addressing modes

readNext :: Emulator Word8
readNext = fetchNext >>= read

accumulator :: Emulator ()
accumulator = pure ()

immediate :: Emulator Word16
immediate = fetchNext

implied :: Emulator Word16
implied = pure 0

zeroPage :: Emulator Word16
zeroPage = (fetchNext >>= read) <&> fromIntegral

zeroPageX :: Emulator Word16
zeroPageX = do
  addr  <- readNext
  xreg  <- readReg x
  return (fromIntegral $ addr + xreg)

zeroPageY :: Emulator Word16
zeroPageY = do
  xreg  <- readReg x
  addr  <- readNext
  return (fromIntegral $ addr + xreg)

absolute :: Emulator Word16
absolute = fetchNext >>= readAddress

addPenaltyCycles :: Bool -> Penalty -> Emulator ()
addPenaltyCycles _    None = pure ()
addPenaltyCycles False _   = pure ()
addPenaltyCycles True BoundaryCross = cycle 1

absoluteGen :: Emulator Word8 -> Penalty -> Emulator Word16
absoluteGen acquire penalty = do
  addr  <- absolute 
  reg16 <- fromIntegral <$> acquire
  let 
    result = addr + reg16
    pageCrossed = addr `onDifferentPage` result
  addPenaltyCycles pageCrossed penalty
  return result

absoluteX :: Emulator Word16
absoluteX = absoluteGen (readReg x) None

absoluteXP :: Emulator Word16
absoluteXP = absoluteGen (readReg x) BoundaryCross

absoluteY :: Emulator Word16
absoluteY = absoluteGen (readReg y) None

absoluteYP :: Emulator Word16
absoluteYP = absoluteGen (readReg y) BoundaryCross

indirect :: Emulator Word16
indirect = fetchNext >>= readAddressWithBug

indirectX :: Emulator Word16
indirectX = do
  addr <- readNext
  xreg <- readReg x
  readAddressWithBug (fromIntegral $ addr + xreg)

indirectYGen :: Penalty -> Emulator Word16
indirectYGen penalty = do
  yreg  <- readReg y
  addr  <- readNext <&> fromIntegral >>= readAddressWithBug
  let
    result = addr + fromIntegral yreg
  addPenaltyCycles (addr `onDifferentPage` result) penalty
  return result

indirectY :: Emulator Word16
indirectY = indirectYGen None

indirectYP :: Emulator Word16
indirectYP = indirectYGen BoundaryCross

relative :: Emulator Word16
relative = do
  pc   <- fetch
  addr <- fetchNext >>= readAddress
  let 
    byte = fromIntegral $ addr .&. 0x00FF
    pageDiff = if byte < 0x80 then 0 else 0x100
  return $ pc + 2 + byte - pageDiff

decodeOpcode :: Word8 -> Opcode
decodeOpcode opcode = case opcode of
  0x00 -> op (implied >>= brk)      1 7;  0x80 -> op (immediate >> nop)     2 2;
  0x01 -> op (indirectX >>= ora)    2 6;  0x81 -> op (indirectX >>= sta)    2 6; 
  0x02 -> op (implied >> kil)       1 0;  0x82 -> op (immediate >> nop)     2 2; 
  0x03 -> op (indirectX >>= slo)    2 8;  0x83 -> op (indirectX >>= sax)    2 6; 
  0x04 -> op (zeroPage >>  nop)     2 3;  0x84 -> op (zeroPage >>= sty)     2 3; 
  0x05 -> op (zeroPage >>= ora)     2 3;  0x85 -> op (zeroPage >>= sta)     2 3; 
  0x06 -> op (zeroPage >>= aslM)    2 5;  0x86 -> op (zeroPage >>= stx)     2 3;
  0x07 -> op (zeroPage >>= slo)     2 5;  0x87 -> op (zeroPage >>= sax)     2 3; 
  0x08 -> op (implied >>  php)      1 3;  0x88 -> op (implied >> dey)       1 2; 
  0x09 -> op (immediate >>= ora)    2 2;  0x89 -> op (immediate >> nop)     2 2; 
  0x0A -> op (accumulator >> aslA)  1 2;  0x8A -> op (implied >> txa)       1 2; 
  0x0B -> op (immediate >>= anc)    2 2;  0x8B -> op (immediate >>= xaa)    2 2; 
  0x0C -> op (absolute >>  nop)     3 4;  0x8C -> op (absolute >>= sty)     3 4;
  0x0D -> op (absolute >>= ora)     3 4;  0x8D -> op (absolute >>= sta)     3 4;
  0x0E -> op (absolute >>= aslM)    3 6;  0x8E -> op (absolute >>= stx)     3 4; 
  0x0F -> op (absolute >>= slo)     3 6;  0x8F -> op (absolute >>= sax)     3 4;
  0x10 -> op (relative >>= bpl)     2 2;  0x90 -> op (relative  >>= bcc)    2 2;
  0x11 -> op (indirectYP >>= ora)   2 5;  0x91 -> op (indirectY >>= sta)    2 6;
  0x12 -> op (implied >> kil)       1 0;  0x92 -> op (implied   >>= kil)    1 0; 
  0x13 -> op (indirectY >>= slo)    2 8;  0x93 -> op (indirectY >>= ahx)    2 6;
  0x14 -> op (zeroPageX >>  nop)    2 4;  0x94 -> op (zeroPageX >>= sty)    2 4; 
  0x15 -> op (zeroPageX >>= ora)    2 4;  0x95 -> op (zeroPageX >>= sta)    2 4; 
  0x16 -> op (zeroPageX >>= aslM)   2 6;  0x96 -> op (zeroPageY >>= stx)    2 4; 
  0x17 -> op (zeroPageX >>= slo)    2 6;  0x97 -> op (zeroPageY >>= sax)    2 4; 
  0x18 -> op (implied >> clc)       1 2;  0x98 -> op (implied   >> tya)     1 2;
  0x19 -> op (absoluteYP >>= ora)   3 4;  0x99 -> op (absoluteY >>= sta)    3 5; 
  0x1A -> op (implied >> nop)       1 2;  0x9A -> op (implied   >> txs)     1 2;
  0x1B -> op (absoluteY  >>= slo)   3 7;  0x9B -> op (absoluteY >>= tas)    3 5;
  0x1C -> op (absoluteX  >>  nop)   3 4;  0x9C -> op (absoluteX >>= shy)    3 5; 
  0x1D -> op (absoluteXP >>= ora)   3 4;  0x9D -> op (absoluteX >>= sta)    3 5; 
  0x1E -> op (absoluteX >>= aslM)   3 7;  0x9E -> op (absoluteY >>= shx)    3 5;
  0x1F -> op (absoluteX >>= slo)    3 7;  0x9F -> op (absoluteY >>= ahx)    3 5;
  0x20 -> op (absolute >>= jsr)     3 6;  0xA0 -> op (immediate >>= ldy)    2 2;
  0x21 -> op (indirectX >>= and)    2 6;  0xA1 -> op (indirectX >>= lda)    2 6;
  0x22 -> op (implied >> kil)       1 0;  0xA2 -> op (immediate >>= ldx)    2 2;
  0x23 -> op (indirectX >>= rla)    2 8;  0xA3 -> op (indirectX >>= lax)    2 6;
  0x24 -> op (zeroPage >>= bit)     2 3;  0xA4 -> op (zeroPage >>= ldy)     2 3;
  0x25 -> op (zeroPage >>= and)     2 3;  0xA5 -> op (zeroPage >>= lda)     2 3;
  0x26 -> op (zeroPage >>= rolM)    2 5;  0xA6 -> op (zeroPage >>= ldx)     2 3;
  0x27 -> op (zeroPage >>= rla)     2 5;  0xA7 -> op (zeroPage >>= lax)     2 3;
  0x28 -> op (implied >> plp)       1 4;  0xA8 -> op (implied  >> tay)      1 2;
  0x29 -> op (immediate >>= and)    2 2;  0xA9 -> op (immediate >>= lda)    2 2;
  0x2A -> op (accumulator >> rolA)  1 2;  0xAA -> op (implied   >> tax)     1 2;
  0x2B -> op (immediate >>= anc)    2 2;  0xAB -> op (immediate >>= lax)    2 2;
  0x2C -> op (absolute >>= bit)     3 4;  0xAC -> op (absolute >>= ldy)     3 4; 
  0x2D -> op (absolute >>= and)     3 4;  0xAD -> op (absolute >>= lda)     3 4; 
  0x2E -> op (absolute >>= rolM)    3 6;  0xAE -> op (absolute >>= ldx)     3 4; 
  0x2F -> op (absolute >>= rla)     3 6;  0xAF -> op (absolute >>= lax)     3 4; 
  0x30 -> op (relative >>= bmi)     2 2;  0xB0 -> op (relative >>= bcs)     2 2; 
  0x31 -> op (indirectYP >>= and)   2 5;  0xB1 -> op (indirectYP >>= lda)   2 5;
  0x32 -> op (implied >> kil)       1 0;  0xB2 -> op (implied >> kil)       1 0; 
  0x33 -> op (indirectY >>= rla)    2 8;  0xB3 -> op (indirectX >> lax)     2 5;
  0x34 -> op (zeroPageX >>  nop)    2 4;  0xB4 -> op (zeroPageX >>= ldy)    2 4;
  0x35 -> op (zeroPageX >>= and)    2 4;  0xB5 -> op (zeroPageX >>= lda)    2 4;
  0x36 -> op (zeroPageX >>= rolM)   2 6;  0xB6 -> op (zeroPageY >>= ldx)    2 4;
  0x37 -> op (zeroPageX >>= rla)    2 6;  0xB7 -> op (zeroPageY >>= lax)    2 4;
  0x38 -> op (implied >> sec)       1 2;  0xB8 -> op (implied >> clv)       1 2;
  0x39 -> op (absoluteYP >>= and)   3 4;  0xB9 -> op (absoluteYP >>= lda)   3 4;
  0x3A -> op (implied >> nop)       1 2;  0xBA -> op (implied >> tsx)       1 2;
  0x3B -> op (absoluteY >>=  rla)   3 7;  0xBB -> op (absoluteY >>= las)    3 4;
  0x3C -> op (absoluteX >>   nop)   3 4;  0xBC -> op (absoluteXP >>= ldy)   3 4;
  0x3D -> op (absoluteXP >>= and)   3 4;  0xBD -> op (absoluteXP >>= lda)   3 4;
  0x3E -> op (absoluteX >>= rolM)   3 7;  0xBE -> op (absoluteYP >>= ldx)   3 4; 
  0x3F -> op (absoluteX >>= rla)    3 7;  0xBF -> op (absoluteY >>= lax)    3 4; 
  0x40 -> op (implied    >> rti)    1 6;  0xC0 -> op (immediate >>= cpy)    2 2;
  0x41 -> op (indirectX  >>= eor)   2 6;  0xC1 -> op (indirectX >>= cmp)    2 6;
  0x42 -> op (implied    >> kil)    1 0;  0xC2 -> op (immediate >>  nop)    2 2;
  0x43 -> op (indirectX  >>= sre)   2 8;  0xC3 -> op (indirectX >>= dcp)    2 8;
  0x44 -> op (zeroPage >>  nop)     2 3;  0xC4 -> op (zeroPage >>= cpy)     2 3;
  0x45 -> op (zeroPage >>= eor)     2 3;  0xC5 -> op (zeroPage >>= cmp)     2 3;
  0x46 -> op (zeroPage >>= lsrM)    2 5;  0xC6 -> op (zeroPage >>= dec)     2 5;
  0x47 -> op (zeroPage >>= sre)     2 5;  0xC7 -> op (zeroPage >>= dcp)     2 5;
  0x48 -> op (implied >>  pha)      1 3;  0xC8 -> op (implied >> iny)       1 2;
  0x49 -> op (immediate >>= eor)    2 2;  0xC9 -> op (immediate >>= cmp)    2 2;
  0x4A -> op (accumulator >> lsrA)  1 2;  0xCA -> op (implied   >> dex)     1 2;
  0x4B -> op (immediate >>= alr)    2 2;  0xCB -> op (immediate >>= axs)    2 2;
  0x4C -> op (absolute >>= jmp)     3 3;  0xCC -> op (absolute  >>= cpy)    3 4;
  0x4D -> op (absolute >>= eor)     3 4;  0xCD -> op (absolute  >>= cmp)    3 4;
  0x4E -> op (absolute >>= lsrM)    3 6;  0xCE -> op (absolute  >>= dec)    3 6;
  0x4F -> op (absolute >>= sre)     3 6;  0xCF -> op (absolute  >>= dcp)    3 6; 
  0x50 -> op (relative >>= bvc)     2 2;  0xD0 -> op (relative  >>= bne)    2 2;
  0x51 -> op (indirectYP >>= eor)   2 5;  0xD1 -> op (indirectYP >>= cmp)   2 5;
  0x52 -> op (implied >> kil)       1 0;  0xD2 -> op (implied >>= kil)      1 0; 
  0x53 -> op (indirectY >> sre)     2 8;  0xD3 -> op (indirectY >>= dcp)    2 8;
  0x54 -> op (zeroPageX  >>  nop)   2 4;  0xD4 -> op (zeroPageX >>  nop)    2 4;
  0x55 -> op (zeroPageX  >>= eor)   2 4;  0xD5 -> op (zeroPageX >>= cmp)    2 4; 
  0x56 -> op (zeroPageX  >>= lsrM)  2 6;  0xD6 -> op (zeroPageX >>= dec)    2 6; 
  0x57 -> op (zeroPageX  >>= sre)   2 6;  0xD7 -> op (zeroPageX >>= dcp)    2 6;
  0x58 -> op (implied >> cli)       1 2;  0xD8 -> op (implied >> cld)       1 2; 
  0x59 -> op (absoluteYP >>= eor)   3 4;  0xD9 -> op (absoluteYP >>= cmp)   3 4;
  0x5A -> op (implied >> nop)       1 2;  0xDA -> op (implied >> nop)       1 2;
  0x5B -> op (absoluteY >>= sre)    3 7;  0xDB -> op (absoluteY >>= dcp)    3 7;
  0x5C -> op (absoluteX >>  nop)    3 4;  0xDC -> op (absoluteX >> nop)     3 4;
  0x5D -> op (absoluteXP >>= eor)   3 4;  0xDD -> op (absoluteXP >>= cmp)   3 4;
  0x5E -> op (absoluteX >>= lsrM)   3 7;  0xDE -> op (absoluteX >>= dec)    3 7; 
  0x5F -> op (absoluteX >>= sre)    3 7;  0xDF -> op (absoluteX >>= dcp)    3 7; 
  0x60 -> op (implied >> rts)       1 6;  0xE0 -> op (immediate >>= cpx)    2 2;
  0x61 -> op (indirectX >>= adc)    2 6;  0xE1 -> op (indirectX >>= sbc)    2 6;
  0x62 -> op (implied >> kil)       1 0;  0xE2 -> op (implied >> nop)       2 2;
  0x63 -> op (indirectX >>= rra)    2 8;  0xE3 -> op (indirectX >>= isc)    2 8;
  0x64 -> op (zeroPage >>  nop)     2 3;  0xE4 -> op (zeroPage >>= cpx)     2 3;
  0x65 -> op (zeroPage >>= adc)     2 3;  0xE5 -> op (zeroPage >>= sbc)     2 3;
  0x66 -> op (zeroPage >>= rorM)    2 5;  0xE6 -> op (zeroPage >>= inc)     2 5;
  0x67 -> op (zeroPage >>= rra)     2 5;  0xE7 -> op (zeroPage >>= isc)     2 5;
  0x68 -> op (implied >> pla)       1 4;  0xE8 -> op (implied >>   inx)     1 2;
  0x69 -> op (immediate >>= adc)    2 2;  0xE9 -> op (immediate >>= sbc)    2 2;
  0x6A -> op (accumulator >> rorA)  1 2;  0xEA -> op (implied >> nop)       1 2;
  0x6B -> op (immediate >>= arr)    2 2;  0xEB -> op (immediate >>= sbc)    2 2;
  0x6C -> op (indirect >>= jmp)     3 5;  0xEC -> op (absolute >>= cpx)     3 4;
  0x6D -> op (absolute >>= adc)     3 4;  0xED -> op (absolute >>= sbc)     3 4; 
  0x6E -> op (absolute >>= rorM)    3 6;  0xEE -> op (absolute >>= inc)     3 6; 
  0x6F -> op (absolute >>= rra)     3 6;  0xEF -> op (absolute >>= isc)     3 6; 
  0x70 -> op (relative >>= bvs)     2 2;  0xF0 -> op (relative >>= beq)     2 2;
  0x71 -> op (indirectYP >>= adc)   2 5;  0xF1 -> op (indirectYP >>= sbc)   2 5;
  0x72 -> op (implied >> kil)       1 0;  0xF2 -> op (implied >>= kil)      1 0;
  0x73 -> op (indirectY >>= rra)    2 8;  0xF3 -> op (indirectY >>= isc)    2 8;
  0x74 -> op (zeroPageX >>  nop)    2 4;  0xF4 -> op (implied >> nop)       2 4;
  0x75 -> op (zeroPageX >>= adc)    2 4;  0xF5 -> op (zeroPageX >>= sbc)    2 4; 
  0x76 -> op (zeroPageX >>= rorM)   2 6;  0xF6 -> op (zeroPageX >>= inc)    2 6; 
  0x77 -> op (zeroPageX >>= rra)    2 6;  0xF7 -> op (zeroPageX >>= isc)    2 6; 
  0x78 -> op (implied >> sei)       1 2;  0xF8 -> op (implied >>= sed)      1 2; 
  0x79 -> op (absoluteYP >>= adc)   3 4;  0xF9 -> op (absoluteYP >>= sbc)   3 4; 
  0x7A -> op (implied >> nop)       1 2;  0xFA -> op (implied >> nop)       1 2;
  0x7B -> op (absoluteY >>= rra)    3 7;  0xFB -> op (absoluteY >> isc)     3 7;
  0x7C -> op (absoluteX >>  nop)    3 4;  0xFC -> op (implied >> nop)       3 4;
  0x7D -> op (absoluteXP >>= adc)   3 4;  0xFD -> op (absoluteXP >>= sbc)   3 4;
  0x7E -> op (absoluteX >>= rorM)   3 7;  0xFE -> op (absoluteX >>= inc)    3 7;
  0x7F -> op (absoluteX >>= rra)    3 7;  0xFF -> op (absoluteX >>= isc)    3 7;

isc = undefined
sed = undefined
kil = undefined
dcp = undefined
lax = undefined
las = undefined
ahx = undefined
shx = undefined
shy = undefined
tas = undefined
sax = undefined
xaa = undefined
rra = undefined
arr = undefined
sre = undefined
alr = undefined
rla = undefined
anc = undefined
slo = undefined
axs = undefined
sec = undefined

nmi :: Emulator ()
nmi = do
  fetch >>= pushAddress
  setFlag BreakCommand False
  setFlag InterruptDisable True
  php
  readAddress 0xFFFA >>= writeReg pc
  cycle 8

irq :: Emulator ()
irq = do
  irqEnable <- not <$> testFlag InterruptDisable
  when irqEnable $ do
    fetch >>= pushAddress
    setFlag BreakCommand False
    setFlag InterruptDisable True
    php
    readAddress 0xFFFE >>= writeReg pc
    cycle 7


processInterrupt = do
  readReg intr <&> (toEnum . fromEnum) >>= \case
    NONE -> pure ()
    NMI  -> nmi
    IRQ  -> irq
  writeReg intr 0

decodeNextOpcode :: Emulator Opcode
decodeNextOpcode = (fetch >>= read) <&> decodeOpcode

clock :: Emulator ()
clock = do
  processInterrupt
  Opcode instruction length cycles <- decodeNextOpcode
  modifyReg pc ((+) $ fromIntegral length)
  cycle cycles
  instruction -- run the instruction





