module Main where

import Control.Monad (when, forM)
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit ( ExitCode(..)
                   , exitWith
                   )
import System.FilePath.Posix ( replaceExtension )
import System.IO ( IOMode(..)
                 , withFile
                 )

import LC3b.Assemble
import LC3b.Utils

main = do
  args <- getArgs

  -- Exit if no file name supplied
  when (null args) $ do
    putStrLn "Supply a program to be assembled"
    exitWith $ ExitFailure 1

  forM args $ \fileName -> do
    progStr <- readFile fileName
    let progTxt = lines progStr
    let (symErr, symTable, ep) = buildSymbolTable progTxt
    let (parseErr, prog) = buildProgram symTable progTxt
    let eBytes = assembleProgram prog
    case (symErr, parseErr, eBytes) of
      (Nothing, Nothing, Right bytes) -> do
        let outFileName = replaceExtension fileName ".out"
        BS.writeFile outFileName (BS.cons (hgh8B ep)
                                  (BS.cons (low8B ep)
                                   bytes))
      (Just e,_,_) ->
        putStrLn ("Error building symbol table:\n" ++ show e)
      (_,Just e,_) ->
        putStrLn ("Error parsing program:\n" ++ show e)
      (_,_,Left e) ->
        putStrLn ("Error assembling program:\n" ++ show e)
