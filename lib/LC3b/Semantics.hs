{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The semantics for the LC-3b state machine
module LC3b.Semantics where

import           Control.Monad (when)
import           Control.Monad.ST (ST)
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Array.ST as ST
import           Data.Array.ST (STArray)
import           Data.Array (Array)
import qualified Data.ByteString as BS
import qualified Data.STRef as ST
import           Data.STRef (STRef)
import           Data.Word (Word8, Word16)
import qualified Data.Bits as B

import LC3b.Utils

-- | The LC3b machine monad
-- We represent the machine state as a bundle of STRefs/STArrays. Since these are
-- essentially pointers, the state itself is not mutable; the ST stuff that the
-- various state pointers point to is.
type MachineM s a = ReaderT (Machine s) (ST s) a

-- | The machine state. This state itself is immutable; it contains references to
-- data in ST.
data Machine s = Machine { pc     :: STRef s Word16
                           -- ^ The program counter
                         , gprs   :: STArray s Word8 Word16
                           -- ^ Eight general purpose registers
                         , memory :: STArray s Word16 Word8
                           -- ^ memory (16-bit address space, byte-addressed)
                         , nzp    :: STRef s (Bool, Bool, Bool)
                           -- ^ condition codes
                         , halted :: STRef s Bool
                           -- ^ for halting
                         }

data SimException = IllFormedException
  deriving Show

-- | Create an initial machine state with no program loaded
initMachine :: ST s (Machine s)
initMachine = do
  -- Initialize everything to 0
  pcRef     <- ST.newSTRef 0x0
  gprsRef   <- ST.newArray (0, 0xF) 0
  memRef    <- ST.newArray (0, 0xFFFF) 0
  nzpRef    <- ST.newSTRef (False, True, False)
  haltedRef <- ST.newSTRef False

  return $
    Machine { pc     = pcRef
            , gprs   = gprsRef
            , memory = memRef
            , nzp    = nzpRef
            , halted = haltedRef
            }

bsInitMachine :: BS.ByteString -> ST s (Either SimException (Machine s))
bsInitMachine bs = case BS.unpack bs of
  (epHgh8 : epLow8 : progBytes) -> do

    -- Initialize everything to 0
    pcRef     <- ST.newSTRef 0x0
    gprsRef   <- ST.newArray (0, 0xF) 0
    memRef    <- ST.newArray (0, 0xFFFF) 0
    nzpRef    <- ST.newSTRef (False, True, False)
    haltedRef <- ST.newSTRef False

    -- set the initial PC
    let ep = (fromIntegral epHgh8 `B.shiftL` 8) B..|. fromIntegral epLow8
    ST.writeSTRef pcRef ep

    -- write the bytestring to the memory
    writeBS ep (BS.pack progBytes) memRef

    return $ return $
      Machine { pc     = pcRef
              , gprs   = gprsRef
              , memory = memRef
              , nzp    = nzpRef
              , halted = haltedRef
              }
  _ -> return $ Left IllFormedException

-- | Execute a MachineM action and return the resulting computation as immutable
-- values. Note that we are still in the ST monad, so this function essentially runs
-- a computation on some initial state and returns the resulting final state of the
-- machine.
execMachine :: MachineM s ()
            -> Machine s
            -> ST s ( Word16
                    , Array Word8 Word16
                    , Array Word16 Word8
                    , (Bool, Bool, Bool)
                    , Bool
                    )
execMachine action = R.runReaderT $ do
  action
  m'      <- R.ask
  pc'     <- lift $ ST.readSTRef  (pc m')
  gprs'   <- lift $ ST.freeze (gprs m')
  mem'    <- lift $ ST.freeze (memory m')
  nzp'    <- lift $ ST.readSTRef  (nzp m')
  halted' <- lift $ ST.readSTRef  (halted m')
  return (pc', gprs', mem', nzp', halted')

----------------------------------------
-- Low-level state transformations.

-- | Get the value of the PC.
readPC :: MachineM s Word16
readPC = do
  machine <- R.ask
  curPC   <- lift $ ST.readSTRef (pc machine)
  return curPC

-- | Write to the PC.
writePC :: Word16 -> MachineM s ()
writePC newPC = do
  machine <- R.ask
  lift $ ST.writeSTRef (pc machine) newPC

-- | Get the value of a register.
-- Guard against out-of-bounds accesses by just zeroing out all but the least 3
-- significant bits.
readReg :: Word8 -> MachineM s Word16
readReg i = do
  machine <- R.ask
  regVal  <- lift $ ST.readArray (gprs machine) i
  return regVal

-- | Write to a register.
writeReg :: Word8 -> Word16 -> MachineM s ()
writeReg i val = do
  machine <- R.ask
  lift $ ST.writeArray (gprs machine) i val

-- | Get the value of a memory location.
readMem :: Word16 -> MachineM s Word8
readMem i = do
  machine <- R.ask
  memVal  <- lift $ ST.readArray (memory machine) i
  return memVal

-- | Get the value of two memory locations as one 16-bit word.
readMem16 :: Word16 -> MachineM s Word16
readMem16 i = do
  machine <- R.ask
  memLow  <- lift $ ST.readArray (memory machine) i
  memHgh  <- lift $ ST.readArray (memory machine) (i+1)
  return $ fromIntegral memLow B..|. (fromIntegral memHgh `B.shiftL` 8)

-- | Write to a memory location.
writeMem ::  Word16 -> Word8 -> MachineM s ()
writeMem i val = do
  machine <- R.ask
  lift $ ST.writeArray (memory machine) i val

-- | Get the values of the condition code registers.
readNZP :: MachineM s (Bool, Bool, Bool)
readNZP = do
  machine <- R.ask
  curNZP  <- lift $ ST.readSTRef (nzp machine)
  return curNZP

-- | Set the condition codes based on a particular value.
setNZP :: Word16 -> MachineM s ()
setNZP val = do
  machine <- R.ask
  lift $ ST.writeSTRef (nzp machine) (n, z, p)
  where n = (val B..&. 0x8000 == 0x8000)
        z = (val == 0x0000)
        p = (val B..&. 0x8000 == 0x0000 && val /= 0x0000)

-- | Is the machine halted?
isHalted :: MachineM s Bool
isHalted = do
  machine <- R.ask
  curHalted <- lift $ ST.readSTRef (halted machine)
  return curHalted

-- | Halt the machine.
halt :: MachineM s ()
halt = do
  machine <- R.ask
  lift $ ST.writeSTRef (halted machine) True

-- | Increment the PC by 2.
incPC :: MachineM s ()
incPC = do
  curPC <- readPC
  writePC (curPC + 2)

-- | Add an arbitrary 16-bit value to the PC.
addPC :: Word16 -> MachineM s ()
addPC offset = do
  curPC <- readPC
  writePC (curPC + offset)

----------------------------------------
-- Transformations for each opcode.
-- I'm abstracting a bit from the definitions of the instructions here. I'm not
-- modeling the immediate expansion (sign extension of a k-bit immediate to 16 bits)
-- or other low-level operations; these transformations assume we have fully known
-- values for everything.

-- | ADD (register variant)
add_reg :: Word8 -> Word8 -> Word8 -> MachineM s ()
add_reg dr sr1 sr2 = do
  sr1_val <- readReg sr1
  sr2_val <- readReg sr2
  let res = sr1_val + sr2_val
  writeReg dr res
  setNZP res
  incPC

-- | ADD (immediate variant)
add_imm :: Word8 -> Word8 -> Word16 -> MachineM s ()
add_imm dr sr1 imm = do
  sr1_val <- readReg sr1
  let res = sr1_val + imm
  writeReg dr res
  setNZP res
  incPC

-- | AND, register variant
and_reg :: Word8 -> Word8 -> Word8 -> MachineM s ()
and_reg dr sr1 sr2 = do
  sr1_val <- readReg sr1
  sr2_val <- readReg sr2
  let res = sr1_val B..&. sr2_val
  writeReg dr res
  setNZP res
  incPC

-- | AND, immediate variant
and_imm :: Word8 -> Word8 -> Word16 -> MachineM s ()
and_imm dr sr1 imm = do
  sr1_val <- readReg sr1
  let res = sr1_val B..&. imm
  writeReg dr res
  setNZP res
  incPC

-- | BR, all variants
br :: Bool -> Bool -> Bool -> Word16 -> MachineM s ()
br checkN checkZ checkP offset = do
  (n, z, p) <- readNZP
  let cond = (n && checkN) || (z && checkZ) || (p && checkP)
  incPC
  when cond (addPC offset)

-- | JMP, all variants
jmp :: Word8 -> MachineM s ()
jmp baser = do
  baser_val <- readReg baser
  writePC baser_val

-- | JSR (JSR pc offset variant)
jsr :: Word16 -> MachineM s ()
jsr offset = do
  curPC <- readPC
  let pc' = curPC + 2 -- incremented PC
  writeReg 7 pc' -- write the incremented PC to R7 for returning
  addPC offset -- set the new PC

-- | JSRR (JSR register variant)
jsrr :: Word8 -> MachineM s ()
jsrr baser = do
  baser_val <- readReg baser
  curPC <- readPC
  let pc' = curPC + 2 -- incremented PC
  writeReg 7 pc' -- write the incremented PC to R7 for returning
  writePC baser_val -- set the new PC

-- | LDB (all variants)
ldb :: Word8 -> Word8 -> Word16 -> MachineM s ()
ldb dr baser offset = do
  baser_val <- readReg baser
  let i = baser_val + offset
  -- read one byte
  mem_val <- readMem i
  let res = sext 8 (fromIntegral mem_val)
  writeReg dr res
  setNZP res
  incPC

-- | LDW (all variants)
ldw :: Word8 -> Word8 -> Word16 -> MachineM s ()
ldw dr baser offset = do
  baser_val <- readReg baser
  let i = baser_val + offset
  -- read two bytes
  res <- readMem16 i
  writeReg dr res
  setNZP res
  incPC

-- | LEA (all variants)
lea :: Word8 -> Word16 -> MachineM s ()
lea dr offset = do
  curPC <- readPC
  let pc' = curPC + 2 -- incremented PC
  let res = pc' + offset
  writeReg dr res
  -- don't set condition codes
  incPC

-- | RET (all variants)
ret :: MachineM s ()
ret = do
  r7_val <- readReg 7
  writePC r7_val

-- | RTI (unimplemented, currently halts machine)
rti :: MachineM s ()
rti = halt

-- | LSHF (SHF left variant)
lshf :: Word8 -> Word8 -> Word8 -> MachineM s ()
lshf dr sr shf = do
  sr_val <- readReg sr
  let res = sr_val `B.shiftL` (fromIntegral $ shf B..&. 0x0f)
  writeReg dr res
  setNZP res
  incPC

-- | RSHFL (SHF right logical variant)
rshfl :: Word8 -> Word8 -> Word8 -> MachineM s ()
rshfl dr sr shf = do
  sr_val <- readReg sr
  let res = (sr_val `B.shiftR` (fromIntegral $ shf B..&. 0x0f))
  writeReg dr res
  setNZP res
  incPC

-- | RSHFA (SHF right arithmetic variant)
rshfa :: Word8 -> Word8 -> Word8 -> MachineM s ()
rshfa dr sr shf = do
  sr_val <- readReg sr
  let res = (sr_val `ashiftR` (fromIntegral $ shf B..&. 0x0f))
  writeReg dr res
  setNZP res
  incPC

-- | STB (all variants)
stb :: Word8 -> Word8 -> Word16 -> MachineM s ()
stb sr baser offset = do
  sr_val    <- readReg sr
  baser_val <- readReg baser
  let addr = baser_val + offset
  writeMem addr (low8B sr_val)
  incPC

-- | STW (all variants)
stw :: Word8 -> Word8 -> Word16 -> MachineM s ()
stw sr baser offset = do
  sr_val    <- readReg sr
  baser_val <- readReg baser
  let addr = baser_val + offset
  writeMem addr (low8B sr_val) -- write lower byte
  writeMem addr (hgh8B sr_val) -- write higher byte
  incPC

-- | TRAP
-- currently halts machine.
trap :: Word16 -> MachineM s ()
trap _ = halt

-- | XOR (register variant)
xor_reg :: Word8 -> Word8 -> Word8 -> MachineM s ()
xor_reg dr sr1 sr2 = do
  sr1_val <- readReg sr1
  sr2_val <- readReg sr2
  let res = sr1_val `B.xor` sr2_val
  writeReg dr res
  setNZP res
  incPC

-- | XOR (immediate variant)
xor_imm :: Word8 -> Word8 -> Word16 -> MachineM s ()
xor_imm dr sr1 imm = do
  sr1_val <- readReg sr1
  let res = sr1_val `B.xor` imm
  writeReg dr res
  setNZP res
  incPC

-- FIXME: Need to report error conditions on illegal opcodes, unimplemented
-- instructions, etc.
-- | Execute a single instruction
stepMachine :: MachineM s ()
stepMachine = do
  -- If the machine has halted, do nothing
  h <- isHalted
  when h $ do
    return ()

  -- Fetch the current instruction
  curPC <- readPC
  instr <- readMem16 curPC

  -- Get the opcode bits
  let opcode = instr `B.shiftR` 12 -- high 4 bits are opcode

  -- Case over the opcode to invoke the correct state handler
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
      let offset = sext 9 pcoffset9 `B.shiftL` 1
      lea dr offset
    15 -> do
      -- TRAP
      let trapvect8 = extract 0 7 instr
      trap trapvect8
    _ -> return ()
  return ()

-- | Run the machine for n instructions, stop early if it halts
stepMachineTillHalted :: Int -> MachineM s ()
stepMachineTillHalted n | n <= 0 = return ()
stepMachineTillHalted n = do
  h <- isHalted
  when (not h) $ do
    stepMachine
    stepMachineTillHalted (n-1)
