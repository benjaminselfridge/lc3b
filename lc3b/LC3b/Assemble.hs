-- Assembler for LC-3b assembly code.

module LC3b.Assemble where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Word (Word8)

assemble :: String -> ByteString
assemble progStr = B.pack $ assembleLine <$> lines progStr

assembleLine :: String -> Word8
assembleLine = undefined
