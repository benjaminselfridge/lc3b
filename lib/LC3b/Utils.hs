module LC3b.Utils
  ( -- * Bit ops
    sext
  , extract
  , ashiftR
  , low8B, hgh8B
  , fitsBits
  , fitsBitsSigned
  -- * ST stuff
  , writeBS
  -- * Miscellaneous
  , prettyHex
  , firstWord
  , wordsToBS
  ) where

import qualified Data.Array.ST as ST
import           Data.Array.ST (STArray, Ix)
import           Control.Monad.ST (ST)
import           Data.Bits ( (.&.)
                           , (.|.)
                           , shiftL
                           , shiftR
                           , complement
                           , setBit
                           , testBit)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Char (isSpace)
import           Data.Word (Word8, Word16)
import           Numeric (showHex)

----------------------------------------
-- Bit ops

-- | Sign-extend a k-bit value to 16 bits.
sext :: Int -> Word16 -> Word16
sext k x = if testBit x (k-1)
           then mask .|. x
           else x
  where mask = complement (setBit 0 k - 1)

-- | Extract a slice from a 16-bit word.
extract :: Integral a => Int -> Int -> Word16 -> a
extract low hgh x = fromIntegral $ x `shiftR` low .&. complement (0xffff `shiftL` (hgh-low+1))

-- | Arithmetic right shift for a 16-bit word. Since Word16 is an unsigned integer
-- representation, the shiftR function from Data.Bits is a logical shift, so we
-- define our own arithmetic shift.
ashiftR :: Word16 -> Int -> Word16
ashiftR x shf = case testBit x 15 of
  True  -> (x `shiftR` shf) .|. (0xffff `shiftL` (16-shf))
  False -> x `shiftR` shf

-- another way to do this:
-- fromIntegral ((fromIntegral (128 :: Word16) :: Int8) `shiftR` shf) :: Word16

-- | Get the least-significant byte from a 16-bit word.
low8B :: Word16 -> Word8
low8B x = fromIntegral (x .&. 0x00ff)

-- | Get the most-significant byte from a 16-bit word.
hgh8B :: Word16 -> Word8
hgh8B x = fromIntegral ((x .&. 0xff00) `shiftR` 8)

-- | Determine if an unsigned integer fits in the specified number of bits. Return
-- False if the input word is negative.
fitsBits :: Integer -> Int -> Bool
fitsBits word width =
  if 0 <= word && word < (1 `shiftL` width)
  then True
  else False

-- | Determine if a signed integer fits in the specified number of bits.
fitsBitsSigned :: Integer -> Int -> Bool
fitsBitsSigned word width = fitsBits adjWord width
  where adjWord = word + (1 `shiftL` (width - 1))

-- FIXME: Num constraint necessary?
-- FIXME: need to account for array out of bounds.
-- | Write a bytestring into an STArray at a pre-specified location.
writeBS :: (Enum i, Num i, Ix i) => i -> ByteString -> STArray s i Word8 -> ST s ()
writeBS ix bs array = do
  case BS.null bs of
    True -> return ()
    _    -> do
      ST.writeArray array ix (BS.head bs)
      writeBS (ix+1) (BS.tail bs) array

-- | Print an integral value in hex with a leading "0x"
prettyHex :: (Show a, Integral a) => a -> String
prettyHex x = "0x" ++ showHex x ""

-- | Given an input string, get the first word and the rest of the string
firstWord :: String -> Maybe (String, String)
firstWord str = do
  let (w, str') = span (not . isSpace) $ dropWhile isSpace str
  case w of
    "" -> Nothing
    _  -> Just (w, str')

-- | Build a ByteString from a list of 16-bit words.
wordsToBS :: [Word16] -> ByteString
wordsToBS ws = BS.pack $ concat $ [ [low8B w, hgh8B w] | w <- ws ]
