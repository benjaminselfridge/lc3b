-- | Assembler for LC-3b programs
module LC3b.Assemble
  ( -- * Types
    ProgramText
  , SymbolTable
  , ParseException(..)
    -- * Symbol table
  , buildSymbolTable
    -- * Assembly
  ) where

import           Control.Monad (when, void)
import           Control.Monad.State (State)
import qualified Control.Monad.State as S
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char (isSpace)
import           Data.List (isPrefixOf)
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

-- | Parsed line of assembly code.
data Line = Line { lineData :: LineData
                 , lineText :: String
                 }

data LineData = LineDataInstr Opcode [Operand]
              | LineDataLiteral Word16

data Operand = OperandRegId RegId
             | OperandImm Imm

data RegId = RegId Word8
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

type Program = [Line]


data ParseException = BadEntryPoint Int String
                    | InvalidOpcode Int String
                    | EmptyLine Int
                    | UnknownSymbol Int String
                    | UnexpectedEOF Int

instance Show ParseException where
  show (BadEntryPoint lineNum line) =
    "  Bad entry point at line " ++ show lineNum ++ ":\n  " ++ line
  show (InvalidOpcode lineNum opcode) =
    "  Invalid opcode at line " ++ show lineNum ++ ":\n  " ++ opcode
  show (EmptyLine lineNum) =
    "  Empty line at line " ++ show lineNum ++ "\n"
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
stbsInit lines = SymbolTableBuilderState 0 1 lines M.empty

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
  st <- lift S.get
  stbIncrLineNum
  lineNum <- stbReadLineNum
  case stbsLines st of
    (line : rst) -> do
      S.modify $ \st -> st { stbsLines = rst }
      return line
    [] -> E.throwE (UnexpectedEOF lineNum)

-- Higher-level parsing functions for the symbol table.

isLabel :: String -> Bool
isLabel s = last s == ':'

-- | Parse a single line of assembly code, looking for a symbol
stbParseLine :: SymbolTableBuilder ()
stbParseLine = do
  lineStr <- stbGetLine
  case words lineStr of
    (label : _) -> do
      when (isLabel label) $ stbAddSymbol label
      -- FIXME: don't increment the PC on a pure comment line
      -- When we get the line parser working we can come back and fix this piece
      stbIncrPC -- since we found a nonempty line, increment the PC
    [] -> return ()

-- | Read the first line of the program to set the entry point. You must call this
-- function *before* calling stbBuildSymbolTable.
stbParseEntryPoint :: SymbolTableBuilder ()
stbParseEntryPoint = do
  lineNum <- stbReadLineNum
  firstLine <- stbGetLine
  case words firstLine of
    [entrStr] -> do
      case readMaybe entrStr of
        Nothing -> do
          E.throwE (BadEntryPoint (lineNum) firstLine)
        Just entryPoint -> stbWritePC entryPoint
    _ -> E.throwE (BadEntryPoint (lineNum) firstLine)

-- | After parsing the entry point of the program with stbParseEntryPoint, call this
-- function to process the rest of the program, building up the symbol table as we go.
stbBuildSymbolTable :: SymbolTableBuilder ()
stbBuildSymbolTable = do
  st <- lift S.get
  case stbsLines st of
    [] -> return ()
    _ -> do stbParseLine
            stbBuildSymbolTable

-- | Build a symbol table from the complete text of the program (including the first
-- line containing the address of the first instruction).
buildSymbolTable :: ProgramText -> (Maybe ParseException, SymbolTable)
buildSymbolTable lines =
  let (e, final_st) = runSTB (stbsInit lines) $ do
        stbParseEntryPoint
        stbBuildSymbolTable
  in case e of
    Right _ -> (Nothing, stbsSymbolTable final_st)
    Left e' -> (Just e', stbsSymbolTable final_st)

----------------------------------------
-- Assembly

-- | Parsing monad for a single line of assembly
-- FIXME: ReaderT with the symbol table and line number?
type LineBuilder = ExceptT ParseException (State String)

runLineBuilder :: String -> LineBuilder a -> (Either ParseException a, String)
runLineBuilder str action = S.runState (E.runExceptT action) str

firstWord :: String -> Maybe (String, String)
firstWord [] = Nothing
firstWord (c:cs) = if isSpace c
  then Just ("", cs)
  else case firstWord cs of
         Nothing -> Nothing
         Just (w, cs') -> Just (c:w, cs')

discardComment :: String -> String
discardComment = takeWhile (/=';')

-- | discard label, if there is one
parseLabel :: Int -> LineBuilder ()
parseLabel lineNum = do
  str <- lift S.get
  case firstWord str of
    Nothing -> E.throwE (EmptyLine lineNum)
    Just (w, ws) -> do
      when (isLabel w) $ lift $ S.put ws

-- | parse opcode
parseOpcode :: Int -> LineBuilder Opcode
parseOpcode lineNum = do
  str <- lift S.get
  case firstWord str of
    Nothing -> E.throwE (EmptyLine lineNum)
    Just (w, ws) -> do
      lift $ S.put ws
      case w of
        "ADD"   -> return ADD
        "AND"   -> return AND
        "BR"    -> return $ BR (True, True, True)
        "BRn"   -> return $ BR (True, False, False)
        "BRz"   -> return $ BR (False, True, False)
        "BRp"   -> return $ BR (False, False, True)
        "BRnz"  -> return $ BR (True, True, False)
        "BRnp"  -> return $ BR (True, False, True)
        "BRzp"  -> return $ BR (False, True, True)
        "BRnzp" -> return $ BR (True, True, True)
        "JMP"   -> return JMP
        "JSR"   -> return JSR
        "LDB"   -> return LDB
        "LDW"   -> return LDW
        "LEA"   -> return LEA
        "RTI"   -> return RTI
        "SHF"   -> return SHF
        "STB"   -> return STB
        "STW"   -> return STW
        "TRAP"  -> return TRAP
        "XOR"   -> return XOR
        _       -> E.throwE (InvalidOpcode lineNum w)

parseOperand :: Int -> SymbolTable -> String -> LineBuilder Operand
parseOperand _ _ "R0" = return $ OperandRegId (RegId 0)
parseOperand _ _ "R1" = return $ OperandRegId (RegId 1)
parseOperand _ _ "R2" = return $ OperandRegId (RegId 2)
parseOperand _ _ "R3" = return $ OperandRegId (RegId 3)
parseOperand _ _ "R4" = return $ OperandRegId (RegId 4)
parseOperand _ _ "R5" = return $ OperandRegId (RegId 5)
parseOperand _ _ "R6" = return $ OperandRegId (RegId 6)
parseOperand _ _ "R7" = return $ OperandRegId (RegId 7)
parseOperand _ _ str | "0x" `isPrefixOf` str = return $ OperandImm (read str)
parseOperand lineNum st str = case M.lookup str st of
  Nothing -> E.throwE $ UnknownSymbol lineNum str
  Just imm -> return $ OperandImm imm

parseOperands :: Int -> SymbolTable -> LineBuilder [Operand]
parseOperands lineNum st = do
  operands <- words <$> lift S.get
  mapM (parseOperand lineNum st) operands

parseInst :: Int -> SymbolTable -> LineBuilder Line
parseInst lineNum st = do
  line <- lift S.get
  parseLabel lineNum
  opcode <- parseOpcode lineNum
  operands <- parseOperands lineNum st
  return $ Line { lineData = LineDataInstr opcode operands
                , lineText = line
                }

parseLiteral :: Int -> SymbolTable -> LineBuilder (Maybe Line)
parseLiteral lineNum st = do
  line <- lift S.get
  case words line of
    [w] -> case readMaybe w of
      Nothing -> return Nothing
      Just lit -> return $ Just $ Line { lineData = LineDataLiteral lit
                                       , lineText = line
                                       }
    _ -> return Nothing

-- | assemble a single line of assembly
parseLine :: Int -> SymbolTable -> LineBuilder Line
parseLine lineNum st = do
  mLit <- parseLiteral lineNum st
  case mLit of
    Just lit -> return lit
    Nothing -> parseInst lineNum st

assembleLine :: Monad m => String -> Int -> SymbolTable -> ExceptT ParseException m Line
assembleLine lineStr lineNum st =
  let (eLine, _) = runLineBuilder lineStr (parseLine lineNum st)
  in case eLine of
       Left e -> E.throwE e
       Right l -> return l

-- | State monad for assembling the program
type ProgramBuilder = ExceptT ParseException (RWS SymbolTable Program ProgramBuilderState)

-- | State for the program builder monad
data ProgramBuilderState =
  ProgramBuilderState { pbsPC :: Word16
                      , pbsLineNum :: Int
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
pbsInit (_:lines) = ProgramBuilderState 0 1 lines
pbsInit _ = ProgramBuilderState 0 1 []

-- Basic functions on ProgramBuilder.

-- | Get the current PC
pbReadPC :: ProgramBuilder Word16
pbReadPC = lift S.get >>= return . pbsPC

-- | Set the current PC
pbWritePC :: Word16 -> ProgramBuilder ()
pbWritePC newPC = lift $ S.modify $ \st -> st { pbsPC = newPC }

-- | Increment the current PC (by 2)
pbIncrPC :: ProgramBuilder ()
pbIncrPC = do
  pc <- pbReadPC
  pbWritePC (pc+2)

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
      S.modify $ \st -> st { pbsLines = rst }
      return line
    [] -> E.throwE (UnexpectedEOF lineNum)

-- | Parse a single line of assembly
pbParseLine :: ProgramBuilder ()
pbParseLine = do
  lineStr <- pbGetLine
  lineNum <- pbReadLineNum
  st <- lift RWS.ask
  line <- assembleLine lineStr lineNum st
  lift $ RWS.tell [line]

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
buildProgram st lines =
  let (e, final_st) = runPB (pbsInit lines) st $ do
        void pbGetLine -- skip first line containing entry point
  in undefined

-- | Build a program from the complete program text
-- buildSymbolTable :: ProgramText -> (Maybe ParseException, SymbolTable)
-- buildSymbolTable lines =
--   let (e, final_st) = runSTB (stbsInit lines) $ do
--         stbParseEntryPoint
--         stbBuildSymbolTable
--   in case e of
--     Right _ -> (Nothing, stbsSymbolTable final_st)
--     Left e' -> (Just e', stbsSymbolTable final_st)
    
