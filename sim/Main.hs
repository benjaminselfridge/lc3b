module Main where

import qualified Data.ByteString as B
import Control.Monad.Trans.State.Lazy
import Control.Monad.Identity
import Control.Monad.ST (ST, runST)
import Data.List (intercalate)
import Data.Array ((!), assocs)
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
      let eMachine = runST $ do
            em <- bsInitMachine progBytes
            case em of
              Left e -> return $ Left e
              Right m -> do
                res <- execMachine (stepMachineTillHalted 100000) m
                return (Right res)

      case eMachine of
        Left IllFormedException -> do
          error "ill-formed binary"
        Right (pc', gprs', _memory', _nzp', _halted') -> do
          putStrLn $ "Final PC: " ++ prettyHex pc'
          putStrLn $ "Final register state:"
          forM_ (assocs gprs') $ \(r, v) -> do
            putStrLn $ "  R[" ++ show r ++ "] = " ++ prettyHex v

