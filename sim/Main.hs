module Main where

import qualified Data.ByteString as B
import Control.Monad.Trans.State.Lazy
import Control.Monad.Identity
import Control.Monad.ST (ST)
import Data.List (intercalate)
import Data.Array ((!))
import Numeric (showHex)
import Data.Word
import System.Environment (getArgs)
import System.Exit ( ExitCode(..)
                   , exitWith
                   )
import System.FilePath.Posix ( replaceExtension )
import System.IO ( IOMode(..)
                 , withFile
                 )

import LC3b.Semantics
import LC3b.Utils

-- FIXME: This all needs to be fixed, it is broken.

bsInitMachine :: B.ByteString -> Either SimException (ST s (Machine s))
bsInitMachine bs = case B.unpack bs of
  (epHgh8 : epLow8 : progBytes) -> return $ do
    return $ undefined
  _ -> Left IllFormedException

data SimException = IllFormedException
  deriving Show

main :: IO ()
main = do
  args <- getArgs

  -- Exit if no file name supplied
  when (null args) $ do
    putStrLn "Supply a binary to be simulated"
    exitWith $ ExitFailure 1

  case args of
    [fileName] -> do
      progBytes <- B.readFile fileName
      let eMachine = bsInitMachine progBytes
      case eMachine of
        Left IllFormedException -> do
          putStrLn "ill-formed binary"
          exitWith $ ExitFailure 1
        Right m -> do
          let (_, m') = undefined -- runMachine m $ stepMachineTillHalted 20
          putStrLn $ undefined -- showMachine m'
