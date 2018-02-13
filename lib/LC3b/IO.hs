module LC3b.IO where

import Data.Word

import LC3b.Machine
import LC3b.Semantics
import LC3b.Utils

currInstr :: Machine -> Word16
currInstr m = fst $ runMachine m $ do
  curPC <- readPC
  instr <- readMem16 curPC
  return instr

stepAndDebug :: Machine -> IO Machine
stepAndDebug m = do
  putStrLn $ "PC = " ++ showHex16 (pc m)
  putStrLn $ "Executing instruction:"
  putStrLn $ "  " ++ showHex16 (currInstr m)
  let (_, m') = runMachine m $ do
        stepMachineTillHalted 1
  putStrLn $ showMachine m'
  return m'
