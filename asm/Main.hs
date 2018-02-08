module Main where

import Control.Monad (when, forM)
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
    let (err, symTable, ep) = buildSymbolTable progTxt
    let (err', prog) = buildProgram symTable progTxt
    case err of
      Nothing -> do putStrLn $ "Program entry point: " ++ show ep
                    putStrLn $ show symTable
                    putStrLn $ show prog
      Just e  -> putStrLn ("Error building symbol table:\n" ++ show e)
