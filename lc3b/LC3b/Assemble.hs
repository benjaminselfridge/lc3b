-- Assembler for LC-3b assembly code.

module LC3b.Assemble where

import           Control.Monad (when)
import           Control.Monad.State (State)
import qualified Control.Monad.State as S
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Word (Word8, Word16)
import           Text.Read (readMaybe)

data Line = InstrLine { lineLabel :: String
                      , lineData :: LineData
                      , lineComment :: String
                      , lineText :: String
                      }

data Opcode = ADD
            | AND
            | BR
            | JMP
            | JSR
            | LDB
            | LDW
            | LEA
            | RTI
            | SHF
            | STB
            | STW
            | TRAP
            | XOR

data Operand = OperandRegId RegId
             | OperandNZP NZP
             | OperandImm Imm

data RegId = RegId
data NZP = NZP
data Imm = Imm

data Instruction = Instruction

data LineData = LineDataInstr Instruction
              | LineDataLiteral Word16

type Program = [Line]

type ProgramText = [String]


-- Build the symbol table
type SymbolTable = Map String Word16

-- monad for building a symbol table
data SymbolTableBuilderState =
  SymbolTableBuilderState { stbsPC :: Word16
                            -- ^ The position of the current line
                          , stbsLines :: ProgramText
                            -- ^ The unparsed lines remaining in the program
                          , stbsSymbolTable :: SymbolTable
                            -- ^ The completed symbol table thus far
                          }

stbsInit :: ProgramText -> SymbolTableBuilderState
stbsInit lines = SymbolTableBuilderState 0 lines M.empty

-- state monad for building the symbol table
type SymbolTableBuilder a = ExceptT ParseException (State SymbolTableBuilderState) a

runSTB :: SymbolTableBuilderState
       -> SymbolTableBuilder a
       -> (Either ParseException a, SymbolTableBuilderState)
runSTB st stb = S.runState (E.runExceptT stb) st

data ParseException = BadEntryPoint String
                    | UnexpectedEOF

-- basic functions on SymbolTableBuilder
stbReadPC :: SymbolTableBuilder Word16
stbReadPC = lift S.get >>= return . stbsPC

stbWritePC :: Word16 -> SymbolTableBuilder ()
stbWritePC newPC = lift $ S.modify $ \st -> st { stbsPC = newPC }

stbIncrPC :: SymbolTableBuilder ()
stbIncrPC = do
  pc <- stbReadPC
  stbWritePC (pc+2)

stbAddSymbol :: String -> SymbolTableBuilder ()
stbAddSymbol label = do
  curPC <- stbReadPC
  S.modify $ \st ->
    st { stbsSymbolTable = M.insert label curPC (stbsSymbolTable st) }

stbGetLine :: SymbolTableBuilder String
stbGetLine = do
  st <- lift S.get
  case stbsLines st of
    (line : rst) -> do
      S.modify $ \st -> st { stbsLines = rst }
      return line
    [] -> E.throwE UnexpectedEOF

-- Read the first line of the program to set the entry point
stbParseEntryPoint :: SymbolTableBuilder ()
stbParseEntryPoint = do
  firstLine <- stbGetLine
  case words firstLine of
    [entrStr] -> do
      case readMaybe entrStr of
        Nothing -> E.throwE (BadEntryPoint firstLine)
        Just entryPoint -> stbWritePC entryPoint
    _ -> E.throwE (BadEntryPoint firstLine)

-- Parse a single line of assembly code, looking for a symbol
stbParseLine :: SymbolTableBuilder ()
stbParseLine = do
  lineStr <- stbGetLine
  case words lineStr of
    (label : _) -> do
      when (last label == ':') $ stbAddSymbol label
      stbIncrPC -- since we found a nonempty line, increment the PC
    [] -> return ()

stbBuildSymbolTable :: SymbolTableBuilder ()
stbBuildSymbolTable = do
  st <- lift S.get
  case stbsLines st of
    [] -> return ()
    _ -> do stbParseLine
            stbBuildSymbolTable

buildSymbolTable :: ProgramText -> SymbolTable
buildSymbolTable lines =
  let (e, final_st) = runSTB (stbsInit lines) stbBuildSymbolTable
  in stbsSymbolTable final_st

-- Build a program, parsing line-by-line

data ProgramBuilder = RWS SymbolTable Program [String]
