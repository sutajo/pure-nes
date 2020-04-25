{-# LANGUAGE DeriveAnyClass #-}

module Nes.CPU.Serialization (
    CPU(..),
    CPURegisters(..),
    serialize,
    deserialize,
    serializeInterruptAccess,
    deserializeInterruptAccess,
    serializeRegisters
) where

import           Data.IORef.Unboxed
import           Data.Serialize
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           GHC.Generics
import           Text.Printf
import           Nes.Emulation.Monad
import           Nes.CPU.InterruptRegisters
import           Nes.CPU.Emulation
import qualified Nes.CPU.Memory      as M
import qualified Nes.Controls        as Controls
import           Nes.Cartridge.Memory (CartridgeAccess)

data SerializedInterruptRegisters = SerializedInterruptRegisters {
  nmi :: Word8,
  irq :: Word8
} deriving (Generic, Serialize)

data CPURegisters = CPURegisters {
  a         ::  Word8, 
  x         ::  Word8, 
  y         ::  Word8, 
  pc        ::  Word16,
  s         ::  Word8, 
  p         ::  Word8, 
  cyc       ::  Int
} deriving (Eq, Generic, Serialize)

data CPU = CPU {
  --Registers
  registers       :: CPURegisters,
  
  interruptAccess ::  SerializedInterruptRegisters,
  ram             ::  VU.Vector Word8,
  controllers     ::  V.Vector Controls.Controller
} deriving (Generic, Serialize)

instance Show CPURegisters where
  show CPURegisters{..} = printf "Cpu { a = 0x%X, x = 0x%X, y = 0x%X, pc = 0x%X, s = 0x%X, p = 0x%X, cycles = %d }" a x y pc s p cyc

serializeInterruptAccess :: Emulator M.CPU SerializedInterruptRegisters
serializeInterruptAccess = 
  SerializedInterruptRegisters  <$>
  readReg (M.nmi . M.interrupts) <*>
  readReg (M.irq . M.interrupts)

deserializeInterruptAccess :: SerializedInterruptRegisters -> IO InterruptRegisters
deserializeInterruptAccess SerializedInterruptRegisters{..} =
  InterruptRegisters <$>
  newIORefU nmi   <*>
  newIORefU irq

serializeRegisters :: Emulator M.CPU CPURegisters
serializeRegisters = 
  CPURegisters <$>
  readReg M.a   <*>
  readReg M.x   <*>
  readReg M.y   <*>
  readReg M.pc  <*>
  readReg M.s   <*>
  readReg M.p   <*>
  readReg M.cyc 

serialize :: Emulator M.CPU CPU 
serialize = 
  CPU           <$>
  serializeRegisters        <*>
  serializeInterruptAccess  <*>
  useMemory M.ram VU.freeze <*>
  useMemory M.controllers V.freeze

deserialize :: 
  CartridgeAccess -> 
  PPUAccess       -> 
  InterruptRegisters -> 
  CPU             -> 
  IO M.CPU
deserialize ca pa ia CPU{..} = 
  let 
    CPURegisters{..} = registers
  in
  M.CPU         <$>
  newIORefU a   <*>
  newIORefU x   <*>
  newIORefU y   <*>
  newIORefU pc  <*>
  newIORefU s   <*>
  newIORefU p   <*>
  newIORefU cyc <*>
  return ia     <*>
  VU.thaw ram   <*>
  return pa     <*>
  return ca     <*>
  V.thaw controllers

