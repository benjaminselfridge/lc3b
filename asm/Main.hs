module Main where

import LC3b.Assemble

program =
  [ "L1:     ADD R0, R0, R1 ; this is a comment"
  , "        ADD R0, R0, R0"
  , "L2:     TRAP"
  ]

main = do
  putStrLn "Assembling test program..."
  let symTable = buildSymbolTable program
  putStrLn (show symTable)
