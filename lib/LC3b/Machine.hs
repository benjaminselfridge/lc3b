-- The machine state of the LC-3b.

module LC3b.Machine where

import qualified Control.Monad.ST as ST
import           Control.Monad.ST (ST)
import qualified Data.Array.ST as ST
import           Data.Array.ST (STArray)
import           Data.ByteString (ByteString)
import           Data.List (intercalate)
import qualified Data.STRef as ST
import           Data.STRef (STRef)
import           Data.Word (Word8, Word16)

-- import LC3b.Utils

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

-- showMachine :: Machine -> String
-- showMachine m =
--   "PC: " ++ showHex16 (pc m) ++ "\n" ++
--   "GPRS: \n" ++
--   foldMap regStr [0..15] ++
--   haltedStr ++
--   "\n"
--   where regStr reg = "  reg[" ++ showHex8 reg ++ "] = " ++ showHex16 (gprs m !! reg) ++ "\n"
--         haltedStr = if (halted m)
--           then "machine is halted."
--           else "machine is running."

-- showMemory :: Machine -> Word16 -> Word16 -> String
-- showMemory m addr bytes =
--   intercalate "\n" addrStrs
--   where addrStrs = map addrStr [addr..addr+bytes-1]
--         addrStr addr' = let val = (memory m) !! addr'
--           in showHex16 addr' ++ ": " ++ showHex8 val
