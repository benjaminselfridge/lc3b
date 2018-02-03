-- The semantics for the LC-3b state machine
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LC3b.Semantics where

import           Control.Monad (when)
import           Control.Monad.Except
import qualified Control.Monad.Trans.State.Lazy as S
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Data.Array as A
import           Data.Word (Word8, Word16)
import qualified Data.Bits as B
import Prelude hiding (not)

import LC3b.Machine
import LC3b.Utils

-- | The LC3b state monad
newtype MachineM m a = MachineM { runMachineM :: StateT Machine m a }
  deriving (Functor,
            Applicative,
            Monad)

-- Base state transformations.
-- | Get the value of the PC.
readPC :: Monad m => MachineM m Word16
readPC = MachineM $ do
  machine <- S.get
  return $ pc machine

-- | Write to the PC.
writePC :: Monad m => Word16 -> MachineM m ()
writePC val = MachineM $ S.modify $ \machine ->
  machine { pc = val }

-- | Get the value of a register.
-- Guard against out-of-bounds accesses by just zeroing out all but the least 3
-- significant bits.
readReg :: Monad m => Word8 -> MachineM m Word16
readReg i = MachineM $ do
  machine <- S.get
  return $ (gprs machine) A.! (i B..&. 0b111)

-- | Write to a register.
writeReg :: Monad m => Word8 -> Word16 -> MachineM m ()
writeReg i val = MachineM $ S.modify $ \machine ->
  machine { gprs = gprs machine A.// [(i B..&. 0b111,val)] }

-- | Get the value of a memory location.
readMem :: Monad m => Word16 -> MachineM m Word8
readMem i = MachineM $ do
  machine <- S.get
  return $ memory machine A.! i

readMem16 :: Monad m => Word16 -> MachineM m Word16
readMem16 i = MachineM $ do
  machine <- S.get
  let low = fromIntegral $ memory machine A.! i
  let hgh = fromIntegral $ memory machine A.! (i+1)
  return $ low B..|. (hgh `B.shiftL` 8)

-- | Write to a memory location.
writeMem :: Monad m => Word16 -> Word8 -> MachineM m ()
writeMem i val = MachineM $ S.modify $ \machine ->
  machine { memory = memory machine A.// [(i,val)] }

-- | Get the values of the condition code registers.
readNZP :: Monad m => MachineM m (Bool, Bool, Bool)
readNZP = MachineM $ do
  machine <- S.get
  return $ nzp machine

-- | Set the condition codes based on a particular value.
setNZP :: Monad m => Word16 -> MachineM m ()
setNZP val = MachineM $ do
  let n = (val B..&. 0x8000 == 0x8000)
  let z = (val == 0x0000)
  let p = (val B..&. 0x8000 == 0x0000)
  S.modify $ \machine ->
    machine { nzp = (n, z, p) }

-- | Is the machine halted?
isHalted :: Monad m => MachineM m Bool
isHalted = MachineM $ do
  machine <- S.get
  return (halted machine)

-- | Halt the machine.
halt :: Monad m => MachineM m ()
halt = MachineM $ S.modify $ \machine ->
  machine { halted = True }

-- Increment the PC by 2.
incPC :: Monad m => MachineM m ()
incPC = do
  curPC <- readPC
  writePC (curPC + 2)

-- Add an arbitrary 16-bit value to the PC.
addPC :: Monad m => Word16 -> MachineM m ()
addPC offset = do
  curPC <- readPC
  writePC (curPC + offset)
-- Transformations for each opcode.
-- I'm abstracting a bit from the definitions of the instructions here. I'm not
-- modeling the immediate expansion (sign extension of a k-bit immediate to 16 bits)
-- or other low-level operations; these transformations assume we have fully known
-- values for everything.

-- ADD (register variant)
add_reg :: Monad m => Word8 -> Word8 -> Word8 -> MachineM m ()
add_reg dr sr1 sr2 = do
  sr1_val <- readReg sr1
  sr2_val <- readReg sr2
  let res = sr1_val + sr2_val
  writeReg dr res
  setNZP res
  incPC

-- add (immediate variant)
add_imm :: Monad m => Word8 -> Word8 -> Word16 -> MachineM m ()
add_imm dr sr1 imm = do
  sr1_val <- readReg sr1
  let res = sr1_val + imm
  writeReg dr res
  setNZP res
  incPC

-- AND, register variant
and_reg :: Monad m => Word8 -> Word8 -> Word8 -> MachineM m ()
and_reg dr sr1 sr2 = do
  sr1_val <- readReg sr1
  sr2_val <- readReg sr2
  let res = sr1_val B..&. sr2_val
  writeReg dr res
  setNZP res
  incPC

-- AND, immediate variant
and_imm :: Monad m => Word8 -> Word8 -> Word16 -> MachineM m ()
and_imm dr sr1 imm = do
  sr1_val <- readReg sr1
  let res = sr1_val B..&. imm
  writeReg dr res
  setNZP res
  incPC

-- BR, all variants
br :: Monad m => Bool -> Bool -> Bool -> Word16 -> MachineM m ()
br checkN checkZ checkP offset = do
  (n, z, p) <- readNZP
  let cond = (n && checkN) || (z && checkZ) || (p && checkP)
  when cond (addPC offset)
  unless cond incPC

-- JMP, all variants
-- when baser == 7, this is considered a RET
jmp :: Monad m => Word8 -> MachineM m ()
jmp baser = do
  baser_val <- readReg baser
  writePC baser_val

-- JSR (JSR pc offset variant)
jsr :: Monad m => Word16 -> MachineM m ()
jsr offset = do
  curPC <- readPC
  let pc' = curPC + 2 -- incremented PC
  writeReg 7 pc' -- write the incremented PC to R7 for returning
  addPC offset -- set the new PC

-- JSRR (JSR register variant)
jsrr :: Monad m => Word8 -> MachineM m ()
jsrr baser = do
  baser_val <- readReg baser
  curPC <- readPC
  let pc' = curPC + 2 -- incremented PC
  writeReg 7 pc' -- write the incremented PC to R7 for returning
  writePC baser_val -- set the new PC

-- LDB (all variants)
ldb :: Monad m => Word8 -> Word8 -> Word16 -> MachineM m ()
ldb dr baser offset = do
  baser_val <- readReg baser
  let i = baser_val + offset
  -- read one byte
  mem_val <- readMem i
  let res = sext 8 (fromIntegral mem_val)
  writeReg dr res
  setNZP res
  incPC

-- LDW (all variants)
ldw :: Monad m => Word8 -> Word8 -> Word16 -> MachineM m ()
ldw dr baser offset = do
  baser_val <- readReg baser
  let i = baser_val + offset
  -- read two bytes
  res <- readMem16 i
  writeReg dr res
  setNZP res
  incPC

-- LEA (all variants)
lea :: Monad m => Word8 -> Word16 -> MachineM m ()
lea dr offset = do
  curPC <- readPC
  let pc' = curPC + 2 -- incremented PC
  let res = pc' + offset
  writeReg dr res
  -- don't set condition codes
  incPC

-- RET (all variants)
ret :: Monad m => MachineM m ()
ret = do
  r7_val <- readReg 7
  writePC r7_val

-- RTI (unimplemented, currently halts machine)
rti :: Monad m => MachineM m ()
rti = halt

-- LSHF (SHF left variant)
lshf :: Monad m => Word8 -> Word8 -> Word8 -> MachineM m ()
lshf dr sr shf = do
  sr_val <- readReg sr
  let res = sr_val `B.shiftL` (fromIntegral $ shf B..&. 0x0f)
  writeReg dr res
  setNZP res
  incPC

-- RSHFL (SHF right logical variant)
rshfl :: Monad m => Word8 -> Word8 -> Word8 -> MachineM m ()
rshfl dr sr shf = do
  sr_val <- readReg sr
  let res = (sr_val `B.shiftR` (fromIntegral $ shf B..&. 0x0f))
  writeReg dr res
  setNZP res
  incPC

-- RSHFA (SHF right arithmetic variant)
rshfa :: Monad m => Word8 -> Word8 -> Word8 -> MachineM m ()
rshfa dr sr shf = do
  sr_val <- readReg sr
  let res = (sr_val `ashiftR` (fromIntegral $ shf B..&. 0x0f))
  writeReg dr res
  setNZP res
  incPC

-- STB (all variants)
stb :: Monad m => Word8 -> Word8 -> Word16 -> MachineM m ()
stb sr baser offset = do
  sr_val    <- readReg sr
  baser_val <- readReg baser
  let addr = baser_val + offset
  writeMem addr (low8B sr_val)
  incPC

-- STW (all variants)
stw :: Monad m => Word8 -> Word8 -> Word16 -> MachineM m ()
stw sr baser offset = do
  sr_val    <- readReg sr
  baser_val <- readReg baser
  let addr = baser_val + offset
  writeMem addr (low8B sr_val) -- write lower byte
  writeMem addr (hgh8B sr_val) -- write higher byte
  incPC

-- TRAP (currently halts machine)
trap :: Monad m => Word16 -> MachineM m ()
trap _ = halt

-- XOR (register variant)
xor_reg :: Monad m => Word8 -> Word8 -> Word8 -> MachineM m ()
xor_reg dr sr1 sr2 = do
  sr1_val <- readReg sr1
  sr2_val <- readReg sr2
  let res = sr1_val `B.xor` sr2_val
  writeReg dr res
  setNZP res
  incPC

-- XOR (immediate variant)
xor_imm :: Monad m => Word8 -> Word8 -> Word16 -> MachineM m ()
xor_imm dr sr1 imm = do
  sr1_val <- readReg sr1
  let res = sr1_val `B.xor` imm
  writeReg dr res
  setNZP res
  incPC

-- FIXME: Need to report error conditions on illegal opcodes, unimplemented
-- instructions, etc.
-- | The step function
stepMachine :: Monad m => MachineM m ()
stepMachine = do
  -- If the machine has halted, do nothing
  halted <- isHalted
  when halted $ do
    return ()

  -- Fetch the current instruction
  curPC <- readPC
  instr <- readMem16 curPC

  -- Get the opcode
  let opcode = instr `B.shiftR` 12 -- high 4 bits are opcode

  -- Case over the opcode to invoke the currect state handler
  case opcode of
    0  -> do
      -- BR
      let n = B.testBit instr 11
      let z = B.testBit instr 10
      let p = B.testBit instr 9
      let pcoffset9 = extract 0 8 instr
      let offset = sext 9 pcoffset9 `B.shiftL` 1
      br n z p offset
    1  -> do
      -- ADD
      let dr  = extract 9 11 instr
      let sr1 = extract 6 8 instr
      -- two variants
      case B.testBit instr 5 of
        False -> do
          let sr2 = extract 0 2 instr
          add_reg dr sr1 sr2
        True -> do
          let imm5 = extract 0 4 instr
          let imm  = sext 5 imm5
          add_imm dr sr1 imm
    2  -> do
      -- LDB
      let dr       = extract 9 11 instr
      let baser    = extract 6 8 instr
      let boffset6 = extract 0 5 instr
      let offset   = sext 6 boffset6
      ldb dr baser offset
    3  -> do
      -- STB
      let sr       = extract 9 11 instr
      let baser    = extract 6 8 instr
      let boffset6 = extract 0 5 instr
      let offset   = sext 6 boffset6
      stb sr baser offset
    4  -> do
      -- JSR
      -- two variants
      case B.testBit instr 11 of
        False -> do
          let baser = extract 6 8 instr
          jsrr baser
        True -> do
          let pcoffset11 = extract 0 10 instr
          let offset = sext 11 pcoffset11
          jsr offset
    5  -> do
      -- AND
      let dr  = extract 9 11 instr
      let sr1  = extract 6 8 instr
      -- two variants
      case B.testBit instr 5 of
        False -> do
          let sr2 = extract 0 2 instr
          and_reg dr sr1 sr2
        True -> do
          let imm5 = extract 0 4 instr
          let imm  = sext 5 imm5
          and_imm dr sr1 imm
    6  -> do
      -- LDW
      let dr      = extract 9 11 instr
      let baser   = extract 6 8 instr
      let offset6 = extract 0 5 instr
      let offset  = sext 6 offset6 `B.shiftL` 1
      ldw dr baser offset
    7  -> do
      -- STW
      let sr      = extract 9 11 instr
      let baser   = extract 6 8 instr
      let offset6 = extract 0 5 instr
      let offset  = sext 6 offset6 `B.shiftL` 1
      stw sr baser offset
    8  -> do
      -- RTI
      rti
    9  -> do
      -- XOR
      let dr  = extract 9 11 instr
      let sr1 = extract 6 8 instr
      -- two variants
      case B.testBit instr 5 of
        False -> do
          let sr2 = extract 0 2 instr
          xor_reg dr sr1 sr2
        True -> do
          let imm5 = extract 0 4 instr
          let imm  = sext 5 imm5
          xor_imm dr sr1 imm
    10 -> halt -- undefined opcode
    11 -> halt -- undefined opcode
    12 -> do
      -- JMP
      let baser = extract 6 8 instr
      jmp baser
    13 -> do
      -- SHF
      let dr = extract 9 11 instr
      let sr = extract 6 8 instr
      let amount4 = low8B $ extract 0 3 instr
      case (B.testBit instr 4, B.testBit instr 5) of
        (False,     _) -> lshf dr sr amount4
        ( True, False) -> rshfl dr sr amount4
        ( True,  True) -> rshfa dr sr amount4
    14 -> do
      -- LEA
      let dr = extract 9 11 instr
      let pcoffset9 = extract 0 8 instr
      let offset = sext 9 pcoffset9
      lea dr offset
    15 -> do
      -- TRAP
      let trapvect8 = extract 0 7 instr
      trap trapvect8
  return ()
