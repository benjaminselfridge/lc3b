module Main where

import Control.Monad (when, forM)
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit ( ExitCode(..)
                   , exitWith
                   )
import System.IO ( IOMode(..)
                 , withFile
                 )

import LC3b.Assemble

main = do
  args <- getArgs

  -- Exit if no file name supplied
  when (null args) $ do
    putStrLn "Supply a program to be assembled"
    exitWith $ ExitFailure 1

  forM args $ \fileName -> do
    putStrLn ("Assembling " ++ fileName ++ "...")
    progStr <- readFile fileName
    let progTxt = lines progStr
    let (symErr, symTable, ep) = buildSymbolTable progTxt
    print symTable
    let (parseErr, prog) = buildProgram symTable progTxt
    let eBytes = assembleProgram prog
    case (symErr, parseErr, eBytes) of
      (Nothing, Nothing, Right bytes) -> do
        putStrLn $ "Program entry point: " ++ show ep
        -- mapM_ print prog
        putStrLn $ "Program bytes: "
        print (BS.unpack bytes)
      (Just e,_,_) ->
        putStrLn ("Error building symbol table:\n" ++ show e)
      (_,Just e,_) ->
        putStrLn ("Error parsing program:\n" ++ show e)
      (_,_,Left e) ->
        putStrLn ("Error assembling program:\n" ++ show e)
