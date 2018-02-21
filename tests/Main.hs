module Main where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.STRef as ST
import qualified Data.Array.ST as ST
import Data.Array ((!))
import Data.Bits (shiftL, (.|.))
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

-- FIXME: We have to actually load the program into memory, which we aren't doing yet.
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
    let ep = (fromIntegral epHgh8 `shiftL` 8) .|. fromIntegral epLow8
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
      let eMachine = runST $ do
            em <- bsInitMachine progBytes
            case em of
              Left e  -> return $ Left e
              Right m -> do
                res <- execMachine (stepMachineTillHalted 100) m
                return (Right res)
      case eMachine of
        Left IllFormedException -> do
          error "ill-formed binary"
        Right (pc', gprs', _memory', _nzp', _halted') -> do
          -- putStrLn $ "Final pc: " ++ show pc'
          forM_ spec $ \req -> case req of
            RegContains rid w -> do
              let rval = gprs' ! rid
              when (rval /= w) $
                error $ "r" ++ show rid ++ " is " ++ showHex16 rval ++ ", should be " ++ showHex16 w

      --     let  = runST $ do
      --           m <- stm
      --           execMachine (stepMachineTillHalted 100) m
