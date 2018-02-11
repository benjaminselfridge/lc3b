module LC3b.Utils where

import qualified Data.Array as A
import           Data.Array (Array, Ix)
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

writeBS :: (Enum i, Ix i) => i -> ByteString -> Array i Word8 -> Array i Word8
writeBS startIx bs array = A.accum amap array bsAlist
  where amap = flip const
        bsAlist = zip [startIx..] (BS.unpack bs)

-- | Sign-extend a k-bit value to 16 bits.
sext :: Int -> Word16 -> Word16
sext k x = if testBit x (k-1)
           then mask .|. x
           else x
  where mask = complement (setBit 0 k - 1)

extract :: Integral a => Int -> Int -> Word16 -> a
extract low hgh x = fromIntegral $ x `shiftR` low .&. complement (0xffff `shiftL` (hgh-low))

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

showHex8 :: Word8 -> String
showHex8 x = "0x" ++ showHex x ""

showHex16 :: Word16 -> String
showHex16 x = "0x" ++ showHex x ""

firstWord :: String -> Maybe (String, String)
firstWord str = do
  let (w, str') = span (not . isSpace) $ dropWhile isSpace str
  case w of
    "" -> Nothing
    _  -> Just (w, str')

wordsToBS :: [Word16] -> ByteString
wordsToBS ws = BS.pack $ concat $ [ [hgh8B w, low8B w] | w <- ws ]
