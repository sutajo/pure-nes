{-# LANGUAGE DeriveAnyClass #-}

module Nes.CPU.Serialization (
    CPU(..),
    serialize,
    deserialize
) where

import           Data.IORef.Unboxed
import           Data.Store
import           GHC.Generics
import           Text.Printf
import           Nes.Emulation.Monad
import           Nes.CPU.Emulation
import qualified Nes.CPU.Memory    as M

data CPU = CPU {
  a         ::  Word8, 
  x         ::  Word8, 
  y         ::  Word8, 
  pc        ::  Word16,
  s         ::  Word8, 
  p         ::  Word8, 
  cyc       ::  Int,
  irqTimer  ::  Word8,
  nmiTimer  ::  Word8
} deriving (Eq, Generic, Store)

instance Show CPU where
  show CPU{..} = printf "Cpu { a = 0x%X, x = 0x%X, y = 0x%X, pc = 0x%X, s = 0x%X, p = 0x%X, cycles = %d }" a x y pc s p cyc

serialize :: Emulator CPU
serialize = 
  CPU           <$>
  readReg M.a   <*>
  readReg M.x   <*>
  readReg M.y   <*>
  readReg M.pc  <*>
  readReg M.s   <*>
  readReg M.p   <*>
  readReg M.cyc <*>
  readReg M.irqTimer <*>
  readReg M.nmiTimer

deserialize :: CPU -> IO M.CPU
deserialize CPU{..} = 
  M.CPU         <$>
  newIORefU a   <*>
  newIORefU x   <*>
  newIORefU y   <*>
  newIORefU pc  <*>
  newIORefU s   <*>
  newIORefU p   <*>
  newIORefU cyc <*>
  newIORefU irqTimer <*>
  newIORefU nmiTimer

