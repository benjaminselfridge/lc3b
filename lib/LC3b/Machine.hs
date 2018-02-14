-- The machine state of the LC-3b.

module LC3b.Machine where

import qualified Data.Array as A
import           Data.Array (Array)
import           Data.ByteString (ByteString)
import           Data.List (intercalate)
import           Data.Word (Word8, Word16)

import LC3b.Utils

data Machine = Machine { pc     :: Word16
                         -- ^ The program counter
                       , gprs   :: Array Word8 Word16
                         -- ^ Eight general purpose registers
                       , memory :: Array Word16 Word8
                         -- ^ memory (16-bit address space, byte-addressed)
                       , nzp :: (Bool, Bool, Bool)
                         -- ^ condition codes
                       , halted :: Bool
                         -- ^ for halting
                       }

initMachine :: Word16 -> ByteString -> Machine
initMachine entry prog =
  Machine { pc = entry
            -- init pc to entry point
          , gprs = emptyRegs
            -- init all GPRs to 0
          , memory = writeBS entry prog emptyMem
            -- init memory to contain program at specified entry point
          , nzp = (False, True, False)
            -- init condition codes to Z
          , halted = False
            -- init in running state
          }
  where emptyRegs = A.array (0, 15)    [(i, 0) | i <- [0..15]]
        emptyMem  = A.array (0, 65535) [(i, 0) | i <- [0..65535]]

showMachine :: Machine -> String
showMachine m =
  "PC: " ++ showHex16 (pc m) ++ "\n" ++
  "GPRS: \n" ++
  foldMap regStr [0..15] ++
  haltedStr ++
  "\n"
  where regStr reg = "  reg[" ++ showHex8 reg ++ "] = " ++ showHex16 (gprs m A.! reg) ++ "\n"
        haltedStr = if (halted m)
          then "machine is halted."
          else "machine is running."

showMemory :: Machine -> Word16 -> Word16 -> String
showMemory m addr bytes =
  intercalate "\n" addrStrs
  where addrStrs = map addrStr [addr..addr+bytes-1]
        addrStr addr' = let val = (memory m) A.! addr'
          in showHex16 addr' ++ ": " ++ showHex8 val
