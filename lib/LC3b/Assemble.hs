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


----------------------------------------
-- Types

-- | Each string is a line, straight from an assembly file.
type ProgramText = [String]

-- | Symbol table mapping labels to addresses in the program
type SymbolTable = Map String Word16

----------------------------------------
-- Symbol table
-- To build the symbol table, we process the program text line by line, incrementing
-- the PC for each nonempty line we encounter. When we see a label, we just add a
-- (String, Word8) pair to the symbol table we are building.

-- | state monad for building the symbol table
type SymbolTableBuilder a = ExceptT ParseException (State SymbolTableBuilderState) a

-- | The state of the symbol table builder
data SymbolTableBuilderState =
  SymbolTableBuilderState { stbsPC :: Word16
                            -- ^ The address of the current instruction
                          , stbsLineNum :: Int
                            -- ^ current line number (for error reporting)
                          , stbsLines :: ProgramText
                            -- ^ The unparsed lines remaining in the program
                          , stbsSymbolTable :: SymbolTable
                            -- ^ The completed symbol table thus far
                          }

stbsInit :: ProgramText -> SymbolTableBuilderState
stbsInit lines = SymbolTableBuilderState 0 1 lines M.empty

runSTB :: SymbolTableBuilderState
       -> SymbolTableBuilder a
       -> (Either ParseException a, SymbolTableBuilderState)
runSTB st stb = S.runState (E.runExceptT stb) st

data ParseException = BadEntryPoint Int String
                    | UnexpectedEOF Int

instance Show ParseException where
  show (BadEntryPoint lineNum line) =
    "  Bad entry point at line " ++ show lineNum ++ ":\n  " ++ line

-- basic functions on SymbolTableBuilder
stbReadPC :: SymbolTableBuilder Word16
stbReadPC = lift S.get >>= return . stbsPC

stbWritePC :: Word16 -> SymbolTableBuilder ()
stbWritePC newPC = lift $ S.modify $ \st -> st { stbsPC = newPC }

stbIncrPC :: SymbolTableBuilder ()
stbIncrPC = do
  pc <- stbReadPC
  stbWritePC (pc+2)

stbReadLineNum :: SymbolTableBuilder Int
stbReadLineNum = lift S.get >>= return . stbsLineNum

stbIncrLineNum :: SymbolTableBuilder ()
stbIncrLineNum = lift $ S.modify $ \st -> st { stbsLineNum = 1 + stbsLineNum st }

stbAddSymbol :: String -> SymbolTableBuilder ()
stbAddSymbol label = do
  curPC <- stbReadPC
  S.modify $ \st ->
    st { stbsSymbolTable = M.insert label curPC (stbsSymbolTable st) }

stbGetLine :: SymbolTableBuilder String
stbGetLine = do
  st <- lift S.get
  stbIncrLineNum
  lineNum <- stbReadLineNum
  case stbsLines st of
    (line : rst) -> do
      S.modify $ \st -> st { stbsLines = rst }
      return line
    [] -> E.throwE (UnexpectedEOF lineNum)

-- Read the first line of the program to set the entry point
stbParseEntryPoint :: SymbolTableBuilder ()
stbParseEntryPoint = do
  firstLine <- stbGetLine
  lineNum <- stbReadLineNum
  case words firstLine of
    [entrStr] -> do
      case readMaybe entrStr of
        Nothing -> do
          E.throwE (BadEntryPoint lineNum firstLine)
        Just entryPoint -> stbWritePC entryPoint
    _ -> E.throwE (BadEntryPoint lineNum firstLine)

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

buildSymbolTable :: ProgramText -> (Maybe ParseException, SymbolTable)
buildSymbolTable lines =
  let (e, final_st) = runSTB (stbsInit lines) $ do
        stbParseEntryPoint
        stbBuildSymbolTable
  in case e of
    Right _ -> (Nothing, stbsSymbolTable final_st)
    Left e' -> (Just e', stbsSymbolTable final_st)

-- Build a program, parsing line-by-line

data ProgramBuilder = RWS SymbolTable Program [String]



-- Assembly

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
