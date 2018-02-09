-- | Assembler for LC-3b programs
module LC3b.Assemble
  ( -- * Types
    ProgramText
  , SymbolTable
  , Line(..)
  , LineData(..)
  , Operand(..)
  , RegId
  , Imm
  , NZP
  , Opcode(..)
  , Program
  , ParseException(..)
    -- * Symbol table
  , buildSymbolTable
    -- * Assembly
  , buildProgram
  ) where

import           Control.Monad (when, void)
import           Control.Monad.State (State)
import qualified Control.Monad.State as S
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import           Data.Char (isSpace)
import           Data.List ( isPrefixOf
                           , stripPrefix)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Word (Word8, Word16)
import           Text.Read (readMaybe)

import LC3b.Utils

import Debug.Trace (trace)

----------------------------------------
-- Types

-- | Each string is a line, straight from an assembly file.
type ProgramText = [String]

-- | Symbol table mapping labels to addresses in the program
type SymbolTable = Map String Word16

-- | Parsed line of assembly code.
data Line = Line { lineData :: LineData
                   -- ^ Parsed line data
                 , lineText :: String
                   -- ^ Original text of the line
                 }
  deriving (Show)

-- | The parsed line data
data LineData = LineDataInstr Opcode [Operand]
              | LineDataLiteral Word16
  deriving (Show)

-- | Operand type, either a register ID or an immediate value.
data Operand = OperandRegId RegId
             | OperandImm Imm
  deriving (Show)

type RegId = Word8
type NZP = (Bool, Bool, Bool)
type Imm = Word16

data Opcode = ADD
            | AND
            | BR NZP
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
  deriving (Show, Read)

type Program = [Line]

data ParseException = BadEntryPoint Int String
                    | InvalidOpcode Int String String
                    | InvalidOperand Int String String
                    | EmptyLine Int
                    | UnknownSymbol Int String
                    | IllFormedLine Int String
                    | UnexpectedEOF Int

instance Show ParseException where
  show (BadEntryPoint lineNum line) =
    "  Bad entry point at line " ++ show lineNum ++ ":\n" ++ line
  show (InvalidOpcode lineNum opcode line) =
    "  Invalid opcode \"" ++ opcode ++ "\" at line " ++ show lineNum ++ ":\n" ++
    line ++ "\n"
  show (InvalidOperand lineNum operand line) =
    "  Invalid operand \"" ++ operand ++ "\" at line " ++ show lineNum ++ ":\n" ++
    line ++ "\n"
  show (EmptyLine lineNum) =
    "  Empty line at line " ++ show lineNum ++ "\n"
  show (IllFormedLine lineNum line) =
    "  Ill-formed line at line " ++ show lineNum ++ ":\n" ++ line ++ "\n"
  show (UnknownSymbol lineNum label) =
    "  Unknown symbol at line " ++ show lineNum ++ ":\n  " ++ label
  show (UnexpectedEOF lineNum) =
    "  Unexpected EOF at line " ++ show lineNum ++ "\n"


----------------------------------------
-- Symbol table builder
-- To build the symbol table, we process the program text line by line, incrementing
-- the PC for each nonempty line we encounter. When we see a label, we just add a
-- (String, Word8) pair to the symbol table we are building.

-- | state/exception monad for building the symbol table
type SymbolTableBuilder a = ExceptT ParseException (State SymbolTableBuilderState) a

-- | The state of the symbol table builder
data SymbolTableBuilderState =
  SymbolTableBuilderState { stbsPC :: Word16
                            -- ^ The address of the current instruction
                          , stbsLineNum :: Int
                            -- ^ line number of first line in the unparsed lines
                            -- remaining in the program (for error reporting)
                          , stbsLines :: ProgramText
                            -- ^ The unparsed lines remaining in the program
                          , stbsSymbolTable :: SymbolTable
                            -- ^ The completed symbol table thus far
                          , stbsEntryPoint :: Word16
                          }

-- | Run a SymbolTableBuilderState computation with some initial state
runSTB :: SymbolTableBuilderState
       -> SymbolTableBuilder a
       -> (Either ParseException a, SymbolTableBuilderState)
runSTB st stb = S.runState (E.runExceptT stb) st

-- | Set up initial state for the SymbolTableBuilder. Notice that we set the PC to 0
-- initially, but before building the symbol table we will read the first line of the
-- program, which should contain a single literal indicating the address that the
-- program should be placed at.
stbsInit :: ProgramText -> SymbolTableBuilderState
stbsInit progLines = SymbolTableBuilderState 0 1 progLines M.empty 0

-- Basic functions on SymbolTableBuilder.

-- | Get the current PC
stbReadPC :: SymbolTableBuilder Word16
stbReadPC = lift S.get >>= return . stbsPC

-- | Set the current PC
stbWritePC :: Word16 -> SymbolTableBuilder ()
stbWritePC newPC = lift $ S.modify $ \st -> st { stbsPC = newPC }

-- | Increment the current PC (by 2)
stbIncrPC :: SymbolTableBuilder ()
stbIncrPC = do
  pc <- stbReadPC
  stbWritePC (pc+2)

-- | Get the current program line number
stbReadLineNum :: SymbolTableBuilder Int
stbReadLineNum = lift S.get >>= return . stbsLineNum

-- | Increment the current program line number
stbIncrLineNum :: SymbolTableBuilder ()
stbIncrLineNum = lift $ S.modify $ \st -> st { stbsLineNum = 1 + stbsLineNum st }

-- | Set entry point
stbWriteEntryPoint :: Word16 -> SymbolTableBuilder ()
stbWriteEntryPoint ep = lift $ S.modify $ \st -> st { stbsEntryPoint = ep }

-- | Insert a symbol into the symbol table, using the current PC as the address for
-- that symbol.
stbAddSymbol :: String -> SymbolTableBuilder ()
stbAddSymbol label = do
  curPC <- stbReadPC
  S.modify $ \st ->
    st { stbsSymbolTable = M.insert label curPC (stbsSymbolTable st) }

-- | Get the next line of the program text, incrementing the line number. If the
-- program text contains no more lines, throw an UnexpectedEOF exception.
stbGetLine :: SymbolTableBuilder String
stbGetLine = do
  stbSt <- lift S.get
  stbIncrLineNum
  lineNum <- stbReadLineNum
  case stbsLines stbSt of
    (line : rst) -> do
      S.modify $ \st -> st { stbsLines = rst }
      return line
    [] -> E.throwE (UnexpectedEOF lineNum)

-- Higher-level parsing functions for the symbol table.

isLabel :: String -> Bool
isLabel [] = False
isLabel s = last s == ':'

-- | Parse a single line of assembly code, looking for a symbol
stbParseLine :: SymbolTableBuilder ()
stbParseLine = do
  lineStr <- stbGetLine
  case words lineStr of
    (label : _) -> do
      when (isLabel label) $ stbAddSymbol (init label)
      -- FIXME: don't increment the PC on a pure comment line
      -- When we get the line parser working we can come back and fix this piece
      stbIncrPC -- since we found a nonempty line, increment the PC
    [] -> return ()

-- | Read the first line of the program to set the entry point. You must call this
-- function *before* calling stbBuildSymbolTable. We also return the entry point for
-- the caller.
stbParseEntryPoint :: SymbolTableBuilder ()
stbParseEntryPoint = do
  lineNum <- stbReadLineNum
  firstLine <- stbGetLine
  case words firstLine of
    [entrStr] -> do
      case readMaybe entrStr of
        Nothing -> do
          E.throwE (BadEntryPoint (lineNum) firstLine)
        Just entryPoint -> do
          stbWritePC entryPoint
          stbWriteEntryPoint entryPoint
    _ -> E.throwE (BadEntryPoint (lineNum) firstLine)

-- | After parsing the entry point of the program with stbParseEntryPoint, call this
-- function to process the rest of the program, building up the symbol table as we go.
stbBuildSymbolTable :: SymbolTableBuilder ()
stbBuildSymbolTable = do
  stbSt <- lift S.get
  case stbsLines stbSt of
    [] -> return ()
    _ -> do stbParseLine
            stbBuildSymbolTable

-- | Build a symbol table from the complete text of the program (including the first
-- line containing the address of the first instruction). Also return the entry point
-- of the program.
buildSymbolTable :: ProgramText -> (Maybe ParseException, SymbolTable, Word16)
buildSymbolTable progLines =
  let (e, final_st) = runSTB (stbsInit progLines) $ do
        stbParseEntryPoint
        stbBuildSymbolTable
  in case e of
    Right _  -> (Nothing, stbsSymbolTable final_st, stbsEntryPoint final_st)
    Left  e' -> (Just e', stbsSymbolTable final_st, stbsEntryPoint final_st)

----------------------------------------
-- Assembly

-- | Simple parser monad for a line of assembly code. Needs an environment including
-- the current line number and the entire original string (for error messages) as
-- well as the symbol table for name resolution. State consists of the suffix of the
-- original string that still has not been parsed.
type LineParser = ExceptT ParseException (RWS (Int, String, SymbolTable) () String)

runLP :: Int -> SymbolTable -> String -> LineParser a -> Either ParseException a
runLP lineNum st line action =
  let (ea, _) = RWS.evalRWS (E.runExceptT action) (lineNum, line, st) line
  in  ea

-- | Discard the label if there is one.
lpDiscardLabel :: LineParser ()
lpDiscardLabel = do
  str <- RWS.get
  let mFirstWord = firstWord str
  case mFirstWord of
    Nothing -> return ()
    Just (w, str') -> do
      -- we know w is nonempty, so it should be safe to call last.
      case last w of
        ':' -> do
          lift $ RWS.put (dropWhile isSpace str')
        _ -> return ()
  return ()

-- | Discard the comment if there is one.
lpDiscardComment :: LineParser ()
lpDiscardComment = do
  str <- RWS.get
  lift $ RWS.put (takeWhile (/= ';') str)

-- | Parse the opcode.
lpParseOpcode :: LineParser Opcode
lpParseOpcode = do
  str <- RWS.get
  (lineNum,line,_) <- RWS.ask
  let mFirstWord = firstWord str
  case mFirstWord of
    Nothing -> do
      E.throwE (IllFormedLine lineNum line)
    Just (w, str') -> do
      -- throw away the first word
      lift $ RWS.put (dropWhile isSpace str')
      -- special case for the BR opcode
      case stripPrefix "BR" w of
        Just cc -> case cc of
          ""    -> return $ BR ( True, True, True)
          "n"   -> return $ BR ( True,False,False)
          "z"   -> return $ BR (False, True, True)
          "p"   -> return $ BR (False,False, True)
          "nz"  -> return $ BR ( True, True,False)
          "np"  -> return $ BR ( True,False, True)
          "zp"  -> return $ BR (False, True, True)
          "nzp" -> return $ BR ( True, True, True)
          _     -> E.throwE (InvalidOpcode lineNum w line)
        Nothing -> do
          case (readMaybe w) of
            Nothing -> E.throwE (InvalidOpcode lineNum w line)
            Just opcode -> return opcode

-- | Parse a single operand (doesn't change the state)
readOperand :: String -> LineParser Operand
readOperand str = do
  (lineNum,line,st) <- RWS.ask
  case str of
    "R0" -> return $ OperandRegId 0
    "R1" -> return $ OperandRegId 1
    "R2" -> return $ OperandRegId 2
    "R3" -> return $ OperandRegId 3
    "R4" -> return $ OperandRegId 4
    "R5" -> return $ OperandRegId 5
    "R6" -> return $ OperandRegId 6
    "R7" -> return $ OperandRegId 7
    _ -> do
      case ("0x" `isPrefixOf` str, M.lookup str st) of
        (True,     _) -> return $ OperandImm (read str)
        (_, Just imm) -> return $ OperandImm imm
        _ -> trace ("<" ++ str ++ ">") $
             E.throwE (InvalidOperand lineNum str line)

-- | Parse the operand list.
lpParseOperands :: LineParser [Operand]
lpParseOperands = do
  str <- RWS.get
  sequence $ readOperand <$> words str

assembleLine :: Int -> SymbolTable -> String -> Either ParseException Line
assembleLine lineNum st lineStr = runLP lineNum st lineStr $ do
  lpDiscardLabel
  lpDiscardComment
  opcode <- lpParseOpcode
  operands <- lpParseOperands
  return $ Line {
    lineData = LineDataInstr opcode operands,
    lineText = lineStr
    }

-- | State monad for assembling the program
type ProgramBuilder = ExceptT ParseException (RWS SymbolTable Program ProgramBuilderState)

-- | State for the program builder monad
data ProgramBuilderState =
  ProgramBuilderState { pbsLineNum :: Int
                      , pbsLines :: ProgramText
                      }

-- | Run a SymbolTableBuilderState computation with some initial state and a
-- pre-computed symbol table
runPB :: ProgramBuilderState
      -> SymbolTable
      -> ProgramBuilder a
      -> (Either ParseException a, Program)
runPB pbSt st pb = RWS.evalRWS (E.runExceptT pb) st pbSt

-- | Set up initial state for the ProgramBuilder.
pbsInit :: ProgramText -> ProgramBuilderState
pbsInit progLines = ProgramBuilderState 1 progLines

-- Basic functions on ProgramBuilder.

-- | Get the current program line number
pbReadLineNum :: ProgramBuilder Int
pbReadLineNum = lift S.get >>= return . pbsLineNum

-- | Increment the current program line number
pbIncrLineNum :: ProgramBuilder ()
pbIncrLineNum = lift $ S.modify $ \st -> st { pbsLineNum = 1 + pbsLineNum st }

-- | Get the next line of the program text, incrementing the line number. If the
-- program text contains no more lines, throw an UnexpectedEOF exception.
pbGetLine :: ProgramBuilder String
pbGetLine = do
  st <- lift S.get
  pbIncrLineNum
  lineNum <- pbReadLineNum
  case pbsLines st of
    (line : rst) -> do
      S.modify $ \pbsSt -> pbsSt { pbsLines = rst }
      return line
    [] -> E.throwE (UnexpectedEOF lineNum)

-- | Parse a single line of assembly
pbParseLine :: ProgramBuilder ()
pbParseLine = do
  lineStr <- pbGetLine
  lineNum <- pbReadLineNum
  st <- lift RWS.ask
  let eline = assembleLine lineNum st lineStr
  case eline of
    Left  e    -> E.throwE e
    Right line -> lift $ RWS.tell [line]

-- | Parse the entire program
pbBuildProgram :: ProgramBuilder ()
pbBuildProgram = do
  st <- lift S.get
  case pbsLines st of
    [] -> return ()
    _ -> do pbParseLine
            pbBuildProgram

-- | Build a program from the complete program text (including first line containing
-- the entry point)
buildProgram :: SymbolTable -> ProgramText -> (Maybe ParseException, Program)
buildProgram st progLines =
  let (e, prog) = runPB (pbsInit progLines) st $ do
        void pbGetLine -- skip first line containing entry point
        pbBuildProgram
  in case e of
    Right _ -> (Nothing, prog)
    Left e' -> (Just e', prog)
