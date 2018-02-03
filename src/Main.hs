module Main where

import qualified Data.ByteString as B
import Control.Monad.Trans.State.Lazy
import Control.Monad.Identity
import Data.List (intercalate)
import Data.Array ((!))
import Numeric (showHex)
import Data.Word

import LC3b.Machine
import LC3b.Semantics

m :: Machine
m = initMachine 0x3000 $ B.pack
  [ 0x21
  , 0x10 -- ADD r0, r0, 0x1
  , 0x00
  , 0x10 -- ADD r0, r0, r0
  , 0x00
  , 0x10 -- ADD r0, r0, r0
  , 0x00
  , 0xf0 -- HALT
  ]

runMachine :: Machine -> MachineM Identity a -> (a, Machine)
runMachine m action = runIdentity $ runStateT (runMachineM action) m

main :: IO ()
main = do
  putStrLn $ showMachine m
  let (_, m')= runMachine m $ do
        stepMachine
        stepMachine
        stepMachine
        stepMachine
  putStrLn $ showMachine m'
  return ()
