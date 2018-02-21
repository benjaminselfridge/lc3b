module LC3b.Utils where

import qualified Data.Array.ST as ST
import           Data.Array.ST (STArray, Ix)
import qualified Control.Monad.ST as ST
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

-- FIXME: Do we really need a Num i constraint here?
writeBS :: (Enum i, Num i, Ix i) => i -> ByteString -> STArray s i Word8 -> ST s (STArray s i Word8)
writeBS ix bs array = do
  case BS.null bs of
    True -> return array
    _    -> do
      ST.writeArray array ix (BS.head bs)
      writeBS (ix+1) (BS.tail bs) array

-- | Sign-extend a k-bit value to 16 bits.
sext :: Int -> Word16 -> Word16
sext k x = if testBit x (k-1)
           then mask .|. x
           else x
  where mask = complement (setBit 0 k - 1)

extract :: Integral a => Int -> Int -> Word16 -> a
extract low hgh x = fromIntegral $ x `shiftR` low .&. complement (0xffff `shiftL` (hgh-low+1))

-- | Word16 arithmetic shift
ashiftR :: Word16 -> Int -> Word16
ashiftR x shf = case testBit x 15 of
  True  -> (x `shiftR` shf) .|. (0xffff `shiftL` (16-shf))
  False -> x `shiftR` shf

-- another way to do this:
-- fromIntegral ((fromIntegral (128 :: Word16) :: Int8) `shiftR` shf) :: Word16

low8B :: Word16 -> Word8
low8B x = fromIntegral (x .&. 0x00ff)

hgh8B :: Word16 -> Word8
hgh8B x = fromIntegral ((x .&. 0xff00) `shiftR` 8)

mkWord16 :: Word8 -> Word8 -> Word16
mkWord16 hgh8 low8 = ((fromIntegral hgh8 :: Word16) `shiftL` 8) .|. fromIntegral low8

showHex8 :: Word8 -> String
showHex8 x = "0x" ++ showHex x ""

-- | FIXME: Rename this
showHex16 :: (Show a, Integral a) => a -> String
showHex16 x = "0x" ++ showHex x ""

firstWord :: String -> Maybe (String, String)
firstWord str = do
  let (w, str') = span (not . isSpace) $ dropWhile isSpace str
  case w of
    "" -> Nothing
    _  -> Just (w, str')

wordsToBS :: [Word16] -> ByteString
wordsToBS ws = BS.pack $ concat $ [ [low8B w, hgh8B w] | w <- ws ]

fitsBits :: Integer -> Int -> Bool
fitsBits word width =
  if 0 <= word &&  word < (1 `shiftL` width)
  then True
  else False

fitsBitsSigned :: Integer -> Int -> Bool
fitsBitsSigned word width =
  let adjWord = word + (1 `shiftL` (width - 1))
  in if 0 <= adjWord &&  adjWord < (1 `shiftL` width)
     then True
     else False
