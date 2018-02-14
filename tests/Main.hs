module Main where

import Control.Monad (forM_)
import Data.Word
import System.FilePath.Glob ( namesMatching )
import System.FilePath.Posix
import Text.Read (readMaybe)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

data Req = RegContains Word8 Word16
  deriving (Show, Read)

-- | A spec is a list of requirements.
type Spec = [Req]

main :: IO ()
main = do
  testSpecs <- namesMatching "tests/asm/*.expected"
  T.defaultMain $ T.testGroup "LC3bTests" [
    asmTests testSpecs
    ]

asmTests :: [FilePath] -> T.TestTree
asmTests = T.testGroup "LC3b" . map mkTest

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ do
  expectedTxt <- readFile fp
  case readMaybe expectedTxt :: Maybe Spec of
    Nothing -> error $ "could not parse spec " ++ fp
    Just spec -> print spec
