{-# LANGUAGE OverloadedStrings #-}

module Nes.CPU.Disassembler (disassemble) where

import Data.Bits
import Data.Word
import Data.Text as Text
import Text.Printf

data Instruction
    = ADC
    | AND
    | ASL
    | BCC 
    | BCS
    | BEQ
    | BIT
    | BMI
    | BNE
    | BPL
    | BRK
    | BVC
    | BVS
    | CLC
    | CLD
    | CLI
    | CLV
    | CMP
    | CPX
    | CPY
    | DEC
    | DEX
    | DEY
    | EOR
    | INC
    | INX
    | INY
    | JMP
    | JSR
    | LDA
    | LDX
    | LDY
    | LSR
    | NOP
    | ORA
    | PHA
    | PHP
    | PLA
    | PLP
    | ROL
    | ROR
    | RTI
    | RTS
    | SBC
    | SEC
    | SED
    | SEI
    | STA
    | STX
    | STY
    | TAX
    | TAY
    | TSX
    | TXA
    | TXS
    | TYA
    | Illegal
    deriving (Show)

data AddressingMode
    = Acc
    | Imm
    | Abs
    | Abx
    | Aby
    | Zp_
    | Zpx
    | Zpy
    | Rel
    | Ind
    | Izx
    | Izy
    | Imp

getArgCount :: AddressingMode -> Int
getArgCount = \case
    Acc -> 0
    Imm -> 1
    Abs -> 2
    Zp_ -> 1
    Zpx -> 1
    Zpy -> 1
    Abx -> 2
    Aby -> 2
    Imp -> 0
    Rel -> 1
    Ind -> 2
    Izx -> 1
    Izy -> 1

renderAddressingMode :: AddressingMode -> (Text -> Text)
renderAddressingMode = \case
    Acc -> \x -> " " <> x <> "A"
    Imm -> \x -> " #" <> x
    Abx -> \x -> " " <> x <> ",X"
    Aby -> \x -> " " <> x <> ",Y"
    Zpx -> \x -> " " <> x <> ",X"
    Zpy -> \x -> " " <> x <> ",Y"
    Izx -> \x -> " (" <> x <> ",X)"
    Izy -> \x -> " (" <> x <> "),Y"
    ___ -> \x -> " " <> x

type DecodedOpcode = (AddressingMode, Instruction)

decodeOpcode :: Word8 -> DecodedOpcode
decodeOpcode opcode = case opcode of
  0x00 -> (Imp ,  BRK)          ;  0x80 -> (Imm ,  NOP)          ;
  0x01 -> (Izx ,  ORA)          ;  0x81 -> (Izx ,  STA)          ; 
  0x02 -> (Imp ,  Illegal)      ;  0x82 -> (Imm ,  NOP)          ; 
  0x03 -> (Izx ,  Illegal)      ;  0x83 -> (Izx ,  Illegal)      ; 
  0x04 -> (Zp_ ,  NOP)          ;  0x84 -> (Zp_ ,  STY)          ; 
  0x05 -> (Zp_ ,  ORA)          ;  0x85 -> (Zp_ ,  STA)          ; 
  0x06 -> (Zp_ ,  ASL)          ;  0x86 -> (Zp_ ,  STX)          ;
  0x07 -> (Zp_ ,  Illegal)      ;  0x87 -> (Zp_ ,  Illegal)      ; 
  0x08 -> (Imp ,  PHP)          ;  0x88 -> (Imp ,  DEY)          ; 
  0x09 -> (Imm ,  ORA)          ;  0x89 -> (Imm ,  NOP)          ; 
  0x0A -> (Acc ,  ASL)          ;  0x8A -> (Imp ,  TXA)          ; 
  0x0B -> (Imm ,  Illegal)      ;  0x8B -> (Imm ,  Illegal)      ; 
  0x0C -> (Abs ,  NOP)          ;  0x8C -> (Abs ,  STY)          ;
  0x0D -> (Abs ,  ORA)          ;  0x8D -> (Abs ,  STA)          ;
  0x0E -> (Abs ,  ASL)          ;  0x8E -> (Abs ,  STX)          ; 
  0x0F -> (Abs ,  Illegal)      ;  0x8F -> (Abs ,  Illegal)      ;
  0x10 -> (Rel ,  BPL)          ;  0x90 -> (Rel  ,  BCC )        ;
  0x11 -> (Izy ,  ORA)          ;  0x91 -> (Izy ,  STA)          ;
  0x12 -> (Imp ,  Illegal)      ;  0x92 -> (Imp   ,  Illegal)    ; 
  0x13 -> (Izy ,  Illegal)      ;  0x93 -> (Izy ,  Illegal)      ;
  0x14 -> (Zpx ,  NOP)          ;  0x94 -> (Zpx ,  STY)          ; 
  0x15 -> (Zpx ,  ORA)          ;  0x95 -> (Zpx ,  STA)          ; 
  0x16 -> (Zpx ,  ASL)          ;  0x96 -> (Zpy ,  STX)          ; 
  0x17 -> (Zpx ,  Illegal)      ;  0x97 -> (Zpy ,  Illegal)      ; 
  0x18 -> (Imp ,  CLC)          ;  0x98 -> (Imp   ,  TYA)        ;
  0x19 -> (Aby ,  ORA)          ;  0x99 -> (Aby ,  STA)          ; 
  0x1A -> (Imp ,  NOP)          ;  0x9A -> (Imp   ,  TXS)        ;
  0x1B -> (Aby ,  Illegal)      ;  0x9B -> (Aby ,  Illegal)      ;
  0x1C -> (Abx ,  NOP)          ;  0x9C -> (Abx ,  Illegal)      ; 
  0x1D -> (Abx ,  ORA)          ;  0x9D -> (Abx ,  STA)          ; 
  0x1E -> (Abx ,  ASL)          ;  0x9E -> (Aby ,  Illegal)      ;
  0x1F -> (Abx ,  Illegal)      ;  0x9F -> (Aby ,  Illegal)      ;
  0x20 -> (Abs ,  JSR)          ;  0xA0 -> (Imm ,  LDY)          ;
  0x21 -> (Izx ,  AND)          ;  0xA1 -> (Izx ,  LDA)          ;
  0x22 -> (Imp ,  Illegal)      ;  0xA2 -> (Imm ,  LDX)          ;
  0x23 -> (Izx ,  Illegal)      ;  0xA3 -> (Izx ,  Illegal)      ;
  0x24 -> (Zp_ ,  BIT)          ;  0xA4 -> (Zp_ ,  LDY)          ;
  0x25 -> (Zp_ ,  AND)          ;  0xA5 -> (Zp_ ,  LDA)          ;
  0x26 -> (Zp_ ,  ROL)          ;  0xA6 -> (Zp_ ,  LDX)          ;
  0x27 -> (Zp_ ,  Illegal)      ;  0xA7 -> (Zp_ ,  Illegal)      ;
  0x28 -> (Imp ,  PLP)          ;  0xA8 -> (Imp  ,  TAY)         ;
  0x29 -> (Imm ,  AND)          ;  0xA9 -> (Imm ,  LDA)          ;
  0x2A -> (Acc ,  ROL)          ;  0xAA -> (Imp   ,  TAX)        ;
  0x2B -> (Imm ,  Illegal)      ;  0xAB -> (Imm ,  Illegal)      ;
  0x2C -> (Abs ,  BIT)          ;  0xAC -> (Abs ,  LDY)          ; 
  0x2D -> (Abs ,  AND)          ;  0xAD -> (Abs ,  LDA)          ; 
  0x2E -> (Abs ,  ROL)          ;  0xAE -> (Abs ,  LDX)          ; 
  0x2F -> (Abs ,  Illegal)      ;  0xAF -> (Abs ,  Illegal)      ; 
  0x30 -> (Rel ,  BMI)          ;  0xB0 -> (Rel ,  BCS)          ; 
  0x31 -> (Izy ,  AND)          ;  0xB1 -> (Izy ,  LDA)          ;
  0x32 -> (Imp ,  Illegal)      ;  0xB2 -> (Imp ,  Illegal)      ; 
  0x33 -> (Izy ,  Illegal)      ;  0xB3 -> (Izy ,  Illegal)      ;
  0x34 -> (Zpx ,  NOP)          ;  0xB4 -> (Zpx ,  LDY)          ;
  0x35 -> (Zpx ,  AND)          ;  0xB5 -> (Zpx ,  LDA)          ;
  0x36 -> (Zpx ,  ROL)          ;  0xB6 -> (Zpy ,  LDX)          ;
  0x37 -> (Zpx ,  Illegal)      ;  0xB7 -> (Zpy ,  Illegal)      ;
  0x38 -> (Imp ,  SEC)          ;  0xB8 -> (Imp ,  CLV)          ;
  0x39 -> (Aby ,  AND)          ;  0xB9 -> (Aby ,  LDA)          ;
  0x3A -> (Imp ,  NOP)          ;  0xBA -> (Imp ,  TSX)          ;
  0x3B -> (Aby ,   Illegal)     ;  0xBB -> (Aby ,  Illegal)      ;
  0x3C -> (Abx ,  NOP)          ;  0xBC -> (Abx ,  LDY)          ;
  0x3D -> (Abx ,  AND)          ;  0xBD -> (Abx ,  LDA)          ;
  0x3E -> (Abx ,  ROL)          ;  0xBE -> (Aby ,  LDX)          ; 
  0x3F -> (Abx ,  Illegal)      ;  0xBF -> (Aby ,  Illegal)      ; 
  0x40 -> (Imp ,  RTI)          ;  0xC0 -> (Imm ,  CPY)          ;
  0x41 -> (Izx ,  EOR  )        ;  0xC1 -> (Izx ,  CMP)          ;
  0x42 -> (Imp ,  Illegal)      ;  0xC2 -> (Imm ,  NOP)          ;
  0x43 -> (Izx ,  Illegal  )    ;  0xC3 -> (Izx ,  Illegal)      ;
  0x44 -> (Zp_ ,  NOP)          ;  0xC4 -> (Zp_ ,  CPY)          ;
  0x45 -> (Zp_ ,  EOR)          ;  0xC5 -> (Zp_ ,  CMP)          ;
  0x46 -> (Zp_ ,  LSR)          ;  0xC6 -> (Zp_ ,  DEC)          ;
  0x47 -> (Zp_ ,  Illegal)      ;  0xC7 -> (Zp_ ,  Illegal)      ;
  0x48 -> (Imp ,  PHA)          ;  0xC8 -> (Imp ,  INY)          ;
  0x49 -> (Imm ,  EOR)          ;  0xC9 -> (Imm ,  CMP)          ;
  0x4A -> (Acc ,  LSR)          ;  0xCA -> (Imp   ,  DEX)        ;
  0x4B -> (Imm ,  Illegal)      ;  0xCB -> (Imm ,  Illegal)      ;
  0x4C -> (Abs ,  JMP)          ;  0xCC -> (Abs  ,  CPY)         ;
  0x4D -> (Abs ,  EOR)          ;  0xCD -> (Abs  ,  CMP)         ;
  0x4E -> (Abs ,  LSR)          ;  0xCE -> (Abs  ,  DEC)         ;
  0x4F -> (Abs ,  Illegal)      ;  0xCF -> (Abs  ,  Illegal)     ; 
  0x50 -> (Rel ,  BVC)          ;  0xD0 -> (Rel  ,  BNE )        ;
  0x51 -> (Izy ,  EOR)          ;  0xD1 -> (Izy ,  CMP)          ;
  0x52 -> (Imp ,  Illegal)      ;  0xD2 -> (Imp ,  Illegal)      ;
  0x53 -> (Izy ,  Illegal)      ;  0xD3 -> (Izy ,  Illegal)      ;
  0x54 -> (Zpx ,  NOP)          ;  0xD4 -> (Zpx ,  NOP)          ;
  0x55 -> (Zpx ,  EOR)          ;  0xD5 -> (Zpx ,  CMP)          ;
  0x56 -> (Zpx ,  LSR  )        ;  0xD6 -> (Zpx ,  DEC)          ;
  0x57 -> (Zpx ,  Illegal)      ;  0xD7 -> (Zpx ,  Illegal)      ;
  0x58 -> (Imp ,  CLI)          ;  0xD8 -> (Imp ,  CLD)          ;
  0x59 -> (Aby ,  EOR)          ;  0xD9 -> (Aby ,  CMP)          ;
  0x5A -> (Imp ,  NOP)          ;  0xDA -> (Imp ,  NOP)          ;
  0x5B -> (Aby ,  Illegal)      ;  0xDB -> (Aby ,  Illegal)      ;
  0x5C -> (Abx ,  NOP)          ;  0xDC -> (Abx ,  NOP)          ;
  0x5D -> (Abx ,  EOR)          ;  0xDD -> (Abx ,  CMP)          ;
  0x5E -> (Abx ,  LSR)          ;  0xDE -> (Abx ,  DEC)          ;
  0x5F -> (Abx ,  Illegal)      ;  0xDF -> (Abx ,  Illegal)      ;
  0x60 -> (Imp ,  RTS)          ;  0xE0 -> (Imm ,  CPX)          ;
  0x61 -> (Izx ,  ADC)          ;  0xE1 -> (Izx ,  SBC)          ;
  0x62 -> (Imp ,  Illegal)      ;  0xE2 -> (Imm ,  NOP)          ;
  0x63 -> (Izx ,  Illegal)      ;  0xE3 -> (Izx ,  Illegal)      ;
  0x64 -> (Zp_ ,  NOP)          ;  0xE4 -> (Zp_ ,  CPX)          ;
  0x65 -> (Zp_ ,  ADC)          ;  0xE5 -> (Zp_ ,  SBC)          ;
  0x66 -> (Zp_ ,  ROR)          ;  0xE6 -> (Zp_ ,  INC)          ;
  0x67 -> (Zp_ ,  Illegal)      ;  0xE7 -> (Zp_ ,  Illegal)      ;
  0x68 -> (Imp ,  PLA)          ;  0xE8 -> (Imp ,   INX)         ;
  0x69 -> (Imm ,  ADC)          ;  0xE9 -> (Imm ,  SBC)          ;
  0x6A -> (Acc ,  ROR)          ;  0xEA -> (Imp ,  NOP)          ;
  0x6B -> (Imm ,  Illegal)      ;  0xEB -> (Imm ,  SBC)          ;
  0x6C -> (Ind ,  JMP)          ;  0xEC -> (Abs ,  CPX)          ;
  0x6D -> (Abs ,  ADC)          ;  0xED -> (Abs ,  SBC)          ; 
  0x6E -> (Abs ,  ROR)          ;  0xEE -> (Abs ,  INC)          ;
  0x6F -> (Abs ,  Illegal)      ;  0xEF -> (Abs ,  Illegal)      ;
  0x70 -> (Rel ,  BVS)          ;  0xF0 -> (Rel ,  BEQ)          ;
  0x71 -> (Izy ,  ADC)          ;  0xF1 -> (Izy ,  SBC)          ;
  0x72 -> (Imp ,  Illegal)      ;  0xF2 -> (Imp ,  Illegal)      ;
  0x73 -> (Izy ,  Illegal)      ;  0xF3 -> (Izy ,  Illegal)      ;
  0x74 -> (Zpx ,  NOP)          ;  0xF4 -> (Zpx ,  NOP)          ;
  0x75 -> (Zpx ,  ADC)          ;  0xF5 -> (Zpx ,  SBC)          ; 
  0x76 -> (Zpx ,  ROR)          ;  0xF6 -> (Zpx ,  INC)          ;
  0x77 -> (Zpx ,  Illegal)      ;  0xF7 -> (Zpx ,  Illegal)      ;
  0x78 -> (Imp ,  SEI)          ;  0xF8 -> (Imp ,  SED)          ;
  0x79 -> (Aby ,  ADC)          ;  0xF9 -> (Aby ,  SBC)          ;
  0x7A -> (Imp ,  NOP)          ;  0xFA -> (Imp ,  NOP)          ;
  0x7B -> (Aby ,  Illegal)      ;  0xFB -> (Aby ,  Illegal)      ;
  0x7C -> (Abx ,  NOP)          ;  0xFC -> (Abx ,  NOP)          ;
  0x7D -> (Abx ,  ADC)          ;  0xFD -> (Abx ,  SBC)          ;
  0x7E -> (Abx ,  ROR)          ;  0xFE -> (Abx ,  INC)          ;
  0x7F -> (Abx ,  Illegal)      ;  0xFF -> (Abx ,  Illegal)      ;


disassembleParts :: Int -> [Word8] -> [Text]
disassembleParts pc [] = []
disassembleParts pc (word : rest) = 
    let 
        (addr, instr) = decodeOpcode word
        argCount      = getArgCount addr
        faddr         = renderAddressingMode addr
        hexText :: Int -> String
        hexText       = printf "$%X"
        toText  :: Int -> Text
        toText        = pack . hexText
        showInstr :: Instruction -> Text
        showInstr     = pack . show
        merge lo hi   = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
        prependInstr  = ((toText pc  <> ":   " <> showInstr instr) <>)
    in case argCount of
        0 -> prependInstr "" : disassembleParts (pc+1) rest

        1 -> case rest of
            (arg : rest') -> (prependInstr . faddr . toText $ fromIntegral arg) : disassembleParts (pc+2) rest'
            _____________ -> []

        2 -> case rest of
            (arg1 : arg2 : rest') -> (prependInstr . faddr . toText $ merge arg1 arg2) : disassembleParts (pc+3) rest'
            _____________________ -> []
    

disassemble :: Word16 -> [Word8] -> Text
disassemble pc = Text.unlines . disassembleParts (fromIntegral pc)