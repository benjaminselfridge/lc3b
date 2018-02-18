module Main where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array ((!))
import qualified  Data.ByteString as BS
import Data.Word
import System.FilePath.Glob ( namesMatching )
import System.FilePath.Posix
import Text.Read (readMaybe)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import LC3b.Assemble
import LC3b.Machine
import LC3b.Semantics
import LC3b.Utils

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

bsInitMachine :: BS.ByteString -> Either SimException (ST s (Machine s))
bsInitMachine bs = case BS.unpack bs of
  (epHgh8 : epLow8 : progBytes) -> return $ do
    return $ Machine {}
  _ -> Left IllFormedException

data SimException = IllFormedException
  deriving Show

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ do
  expectedTxt <- readFile fp
  case readMaybe expectedTxt :: Maybe Spec of
    Nothing -> error $ "could not parse spec " ++ fp
    Just spec -> do
      let fpAsm = replaceExtension fp ".asm"
      asmTxt <- readFile fpAsm
      let progTxt = lines asmTxt
      let (symErr, symTable, ep) = buildSymbolTable progTxt
      -- putStrLn (showSymbolTable symTable)
      let (parseErr, prog) = buildProgram symTable progTxt
      let eBytes = assembleProgram prog
      let outFileName = replaceExtension fpAsm ".out"
      case (symErr, parseErr, eBytes) of
        (Nothing, Nothing, Right bytes) -> do
          BS.writeFile outFileName (BS.cons (hgh8B ep)
                                    (BS.cons (low8B ep)
                                     bytes))
        (Just e,_,_) ->
          error ("Error building symbol table:\n" ++ show e)
        (_,Just e,_) ->
          error ("Error parsing program:\n" ++ show e)
        (_,_,Left e) ->
          error ("Error assembling program:\n" ++ show e)

      -- We've written the binary
      progBytes <- BS.readFile outFileName
      let eMachine = bsInitMachine progBytes
      case eMachine of
        Left IllFormedException -> do
          error "ill-formed binary"
        Right m -> do
          let (_, m') = undefined -- runMachine m $ stepMachineTillHalted 100
          forM_ spec $ \req -> case req of
            RegContains rid w -> do
              undefined
              -- let rval = gprs m' ! rid
              -- when (rval /= w) $
              --   error $ "r" ++ show rid ++ " is " ++ showHex16 rval ++ ", should be " ++ showHex16 w
