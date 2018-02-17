-- | Assembler for LC-3b programs
-- We export three functions: buildSymbolTable, buildProgram, and assembleProgram.
module LC3b.Assemble
  ( -- * Types
    ProgramText
  , SymbolTable
  , Line(..)
  , LineData(..)
  , Operand(..)
  , RegId
  , Imm
  , Opcode(..)
  , Program
  , ParseException(..)
    -- * Functions
  , buildSymbolTable
  , showSymbolTable
  , buildProgram
  , assembleProgram
  , placeBits
  ) where

import           Control.Monad (when)
import           Control.Monad.State (State)
import qualified Control.Monad.State as S
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import           Data.Bits ( (.&.)
                           , (.|.)
                           , shiftL
                           , bit
                           )
import           Data.ByteString (ByteString)
import           Data.Char (isSpace)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Word (Word8, Word16)
import           Text.Read (readMaybe)

import LC3b.Utils

-- FIXME: New plan: parse immediates as full integers, unbounded width. Then,
-- determine whether the given integer is an appropriate for that operand in the
-- assembleLine function. For offsets and immediate arithmetic operands, we check
-- that the signed representation of the integer can fit in the given number of
-- bits. For shift amounts, do the same check for unsigned (and that it is
-- positive). This should enable us to pretty easily report useful errors (warnings?)
-- if the immediates are not appropriately sized or signed.

-- I had a thought -- add a writer to all of this that basically accumulates a list
-- of warnings encountered. That would be cool.

----------------------------------------
-- Types

-- | Each string is a line, straight from an assembly file.
type ProgramText = [String]

-- | Symbol table mapping labels to addresses in the program
type SymbolTable = Map String Word16

showSymbolTable :: SymbolTable -> String
showSymbolTable st = show (showHex16 <$> st)

-- | Parsed line of assembly code.
data Line = Line { lineData :: LineData
                   -- ^ Parsed line data
                 , lineNum  :: Int
                   -- ^ line number for error reporting
                 , lineText :: String
                   -- ^ Original text of the line
                 , lineAddr :: Word16
                   -- ^ address of line in memory
                 }
  deriving (Show)

-- | The parsed line data.
-- Each instruction is just an opcode with a list of operands. The operand list is
-- allowed to be ill-formed; in that case, we will catch the bug while we are
-- outputting to bytes.
data LineData = LineDataInstr Opcode [Operand]
              | LineDataLiteral Word16
  deriving (Show)

-- | Operand type, either a register ID or an immediate value.
data Operand = OperandRegId RegId
             | OperandImm Imm
  deriving (Show)

-- | A register identifier (values between 0 and 7)
type RegId = Word8
-- | A 16-bit immediate value
type Imm = Word16

-- | We create one constructor for each assembly language opcode. Some of these are
-- synonyms.
data Opcode = ADD
            | AND
            | BR
            | BRn
            | BRz
            | BRp
            | BRnz
            | BRnp
            | BRzp
            | BRnzp
            | JMP
            | JSR
            | JSRR
            | LDB
            | LDW
            | LEA
            | NOT
            | RET
            | RTI
            | LSHF
            | RSHFL
            | RSHFA
            | STB
            | STW
            | TRAP
            | XOR
  deriving (Show, Read)

-- | Internal representation for an assembly language program.
type Program = [Line]

-- | The type for assembler exceptions.
data ParseException = BadEntryPoint Int String
                    | InvalidOpcode Int String String
                    | InvalidOperand Int String String
                    | OperandTypeError Int String
                    | OperandWidthError Int Word16 Int String
                    | UnknownSymbol Int String
                    | IllFormedLine Int String
                    | UnexpectedEOF Int
                    | OtherException String

instance Show ParseException where
  show (BadEntryPoint ln line) =
    "  Bad entry point at line " ++ show ln ++ ":\n" ++ line
  show (InvalidOpcode ln opcode line) =
    "  Invalid opcode \"" ++ opcode ++ "\" at line " ++ show ln ++ ":\n" ++
    line ++ "\n"
  show (InvalidOperand ln operand line) =
    "  Invalid operand \"" ++ operand ++ "\" at line " ++ show ln ++ ":\n" ++
    line ++ "\n"
  show (OperandTypeError ln line) =
    "  Opcode/operand type mismatch at line " ++ show ln ++ ":\n" ++
    line ++ "\n"
  show (OperandWidthError ln operand width line) =
    "  Operand width error at line " ++ show ln ++ ":\n" ++
    line ++ "\n" ++
    "  " ++ showHex16 operand ++ " should be a " ++ show width ++ "-bit value"
  show (IllFormedLine ln line) =
    "  Ill-formed line at line " ++ show ln ++ ":\n" ++ line ++ "\n"
  show (UnknownSymbol ln label) =
    "  Unknown symbol at line " ++ show ln ++ ":\n  " ++ label
  show (UnexpectedEOF ln) =
    "  Unexpected EOF at line " ++ show ln ++ "\n"
  show (OtherException s) =
    "  " ++ s


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
  ln <- stbReadLineNum
  case stbsLines stbSt of
    (line : rst) -> do
      S.modify $ \st -> st { stbsLines = rst }
      return line
    [] -> E.throwE (UnexpectedEOF ln)

-- Higher-level parsing functions for the symbol table.

isLabel :: String -> Bool
isLabel [] = False
isLabel s = last s == ':'

-- | Parse a single line of assembly code, looking for a symbol
stbParseLine :: SymbolTableBuilder ()
stbParseLine = do
  lineStr <- stbGetLine
  case words lineStr of
    -- If the line contains only a comment or whitespace, skip it entirely.
    [] -> return ()
    ((';':_):_) -> return ()
    (label : _) -> do
      when (isLabel label) $ stbAddSymbol (init label)
      stbIncrPC

-- | Read the first line of the program to set the entry point. You must call this
-- function *before* calling stbBuildSymbolTable. We also return the entry point for
-- the caller.
stbParseEntryPoint :: SymbolTableBuilder ()
stbParseEntryPoint = do
  ln <- stbReadLineNum
  firstLine <- stbGetLine
  case words firstLine of
    [entrStr] -> do
      case readMaybe entrStr of
        Nothing -> do
          E.throwE (BadEntryPoint ln firstLine)
        Just entryPoint -> do
          stbWritePC entryPoint
          stbWriteEntryPoint entryPoint
    _ -> E.throwE (BadEntryPoint ln firstLine)

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
-- Parsing

-- | Simple parser monad for a line of assembly code. Needs an environment including
-- the current line number and the entire original string (for error messages) as
-- well as the symbol table for name resolution. State consists of the suffix of the
-- original string that still has not been parsed.
type LineParser = ExceptT ParseException (RWS (Int, String, SymbolTable) () String)

runLP :: Int -> SymbolTable -> String -> LineParser a -> Either ParseException a
runLP ln st line action =
  let (ea, _) = RWS.evalRWS (E.runExceptT action) (ln, line, st) line
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
      case isLabel w of
        True -> do
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
  (ln,line,_) <- RWS.ask
  let mFirstWord = firstWord str
  case mFirstWord of
    Nothing -> do
      E.throwE (IllFormedLine ln line)
    Just (w, str') -> do
      -- throw away the first word
      lift $ RWS.put (dropWhile isSpace str')
      case (readMaybe w) of
        Nothing -> E.throwE (InvalidOpcode ln w line)
        Just opcode -> return opcode

-- | Parse a single operand (doesn't change the state)
-- FIXME: When reading labels, we need to sometimes store the offset and sometimes
-- store the immediate.
readOperand :: String -> LineParser Operand
readOperand str = do
  (ln,line,st) <- RWS.ask
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
      case (readMaybe str, M.lookup str st) of
        (Just imm, _) -> return $ OperandImm imm
        (_, Just imm) -> return $ OperandImm imm
        _ -> E.throwE (InvalidOperand ln str line)

-- | Parse the operand list.
lpParseOperands :: LineParser [Operand]
lpParseOperands = do
  str <- RWS.get
  sequence $ readOperand <$> words str

lpParseImm :: Int -> Word16 -> String -> LineParser (Maybe Line)
lpParseImm ln la lineStr = do
  str <- RWS.get
  case words str of
    [ s ] | Just imm <- (readMaybe s :: Maybe Word16) -> do
              return $ Just $ Line { lineData = LineDataLiteral imm
                                   , lineNum  = ln
                                   , lineText = lineStr
                                   , lineAddr = la
                                   }
    _ -> return Nothing

parseLine :: Int -> Word16 -> SymbolTable -> String -> Either ParseException Line
parseLine ln la st lineStr = runLP ln st lineStr $ do
  lpDiscardLabel
  lpDiscardComment

  -- First, attempt to parse the line as an immediate value; if this fails, it should
  -- be a valid line of assembly code
  mImmLine <- lpParseImm ln la lineStr
  case mImmLine of
    Just l -> return l
    Nothing -> do
      opcode <- lpParseOpcode
      operands <- lpParseOperands
      return $ Line {
        lineData = LineDataInstr opcode operands,
        lineNum  = ln,
        lineText = lineStr,
        lineAddr = la
        }

-- | State monad for assembling the program
type ProgramBuilder = ExceptT ParseException (RWS SymbolTable Program ProgramBuilderState)

-- | State for the program builder monad
data ProgramBuilderState =
  ProgramBuilderState { pbsLineNum :: Int
                      , pbsLineAddr :: Word16
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
pbsInit progLines = ProgramBuilderState 0x0 1 progLines

-- Basic functions on ProgramBuilder.

-- | Get the current program line number
pbReadLineNum :: ProgramBuilder Int
pbReadLineNum = lift S.get >>= return . pbsLineNum

-- | Increment the current program line number
pbIncrLineNum :: ProgramBuilder ()
pbIncrLineNum = lift $ S.modify $ \st -> st { pbsLineNum = 1 + pbsLineNum st }

-- | Get the current line address
pbReadLineAddr :: ProgramBuilder Word16
pbReadLineAddr = lift S.get >>= return . pbsLineAddr

-- | Set the current line address
pbWriteLineAddr :: Word16 -> ProgramBuilder ()
pbWriteLineAddr la = lift $ S.modify $ \st -> st { pbsLineAddr = la }

-- | Increment the current line address
pbIncrLineAddr :: ProgramBuilder ()
pbIncrLineAddr = lift $ S.modify $ \st -> st { pbsLineAddr = 2 + pbsLineAddr st }

-- | Get the next line of the program text, incrementing the line number but not the
-- line address. If the program text contains no more lines, throw an UnexpectedEOF
-- exception.
pbGetLine :: ProgramBuilder String
pbGetLine = do
  st <- lift S.get
  ln <- pbReadLineNum
  case pbsLines st of
    (line : rst) -> do
      S.modify $ \pbsSt -> pbsSt { pbsLines = rst }
      pbIncrLineNum
--      pbIncrLineAddr
      return line
    [] -> E.throwE (UnexpectedEOF ln)

-- | Parse the first line of the program text containing the entry point
pbParseEntryPoint :: ProgramBuilder ()
pbParseEntryPoint = do
  ln <- pbReadLineNum -- should be 1
  firstLine <- pbGetLine
  case words firstLine of
    [entrStr] -> do
      case readMaybe entrStr of
        Nothing -> do
          E.throwE (BadEntryPoint ln firstLine)
        Just entryPoint -> do
          pbWriteLineAddr entryPoint
    _ -> E.throwE (BadEntryPoint ln firstLine)

-- | Parse a single line of assembly
pbParseLine :: ProgramBuilder ()
pbParseLine = do
  ln <- pbReadLineNum
  la <- pbReadLineAddr
  lineStr <- pbGetLine
  st <- lift RWS.ask
  case words lineStr of
    [] -> return ()
    ((';':_):_) -> return ()
    _  -> do
      let eline = parseLine ln la st lineStr
      case eline of
        Left  e    -> E.throwE e
        Right line -> do
          lift $ RWS.tell [line]
          pbIncrLineAddr

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
        pbParseEntryPoint
        pbBuildProgram
  in case e of
    Right _ -> (Nothing, prog)
    Left e' -> (Just e', prog)

----------------------------------------
-- Assembly

-- FIXME: we need to actually check the absolute value of the operand here to
-- determine if the bit width is too small. We can't just check equality after
-- masking.
placeBits :: Int -> String -> Int -> Int -> Word16 -> Either ParseException Word16
placeBits ln lineStr lo hi w = do
  let bits = hi - lo + 1
  let w' = w .&. (bit bits - 1)
  if (fromIntegral w) `fitsBits` bits
    then return $ (w .&. (bit bits - 1)) `shiftL` lo
    else Left $ OperandWidthError ln w bits lineStr

  -- show (OperandWidthError ln operand width line) =
  --   "  Operand width error at line " ++ show ln ++ ":\n" ++
  --   line ++ "\n" ++
  --   "  " ++ showHex16 operand ++ " should be a " ++ show width ++ "-bit value"


assembleLine :: Line -> Either ParseException Word16
assembleLine (Line (LineDataInstr opcode operands) ln lineStr la) =
  case (opcode, operands) of
    (ADD, [ OperandRegId dr
          , OperandRegId sr1
          , OperandRegId sr2 ] ) -> do
      opBits  <- placeBits ln lineStr 12 15 0x1
      drBits  <- placeBits ln lineStr 9  11 (fromIntegral dr)
      sr1Bits <- placeBits ln lineStr 6  8  (fromIntegral sr1)
      sr2Bits <- placeBits ln lineStr 0  2  (fromIntegral sr2)
      selBit  <- placeBits ln lineStr 5  5  0x0
      return $ opBits .|. drBits .|. sr1Bits .|. sr2Bits .|. selBit
    (ADD, [ OperandRegId dr
          , OperandRegId sr1
          , OperandImm imm5 ] ) -> do
      opBits  <- placeBits ln lineStr 12 15 0x1
      drBits  <- placeBits ln lineStr 9  11 (fromIntegral dr)
      sr1Bits <- placeBits ln lineStr 6  8  (fromIntegral sr1)
      immBits <- placeBits ln lineStr 0  4  (fromIntegral imm5)
      selBit  <- placeBits ln lineStr 5  5  0x1
      return $ opBits .|. drBits .|. sr1Bits .|. immBits .|. selBit
    (AND, [ OperandRegId dr
          , OperandRegId sr1
          , OperandRegId sr2 ] ) -> do
      opBits  <- placeBits ln lineStr 12 15 0x5
      drBits  <- placeBits ln lineStr 9  11 (fromIntegral dr)
      sr1Bits <- placeBits ln lineStr 6  8  (fromIntegral sr1)
      sr2Bits <- placeBits ln lineStr 0  2  (fromIntegral sr2)
      selBit  <- placeBits ln lineStr 5  5  0x0
      return $ opBits .|. drBits .|. sr1Bits .|. sr2Bits .|. selBit
    (AND, [ OperandRegId dr
          , OperandRegId sr1
          , OperandImm imm5 ] ) -> do
      opBits  <- placeBits ln lineStr 12 15 0x5
      drBits  <- placeBits ln lineStr 9  11 (fromIntegral dr)
      sr1Bits <- placeBits ln lineStr 6  8  (fromIntegral sr1)
      immBits <- placeBits ln lineStr 0  4  (fromIntegral imm5)
      selBit  <- placeBits ln lineStr 5  5  0x1
      return $ opBits .|. drBits .|. sr1Bits .|. immBits .|. selBit
    (BR, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x7
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (BRn, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x4
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (BRz, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x2
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (BRp, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x1
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (BRnz, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x6
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (BRnp, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x5
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (BRzp, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x3
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (BRnzp, [ OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x0
      nzpBits <- placeBits ln lineStr 9 11 0x7
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. nzpBits .|. offBits
    (JMP, [ OperandRegId baser ]) -> do
      opBits    <- placeBits ln lineStr 12 15 0xC
      baserBits <- placeBits ln lineStr 6 8 (fromIntegral baser)
      return $ opBits .|. baserBits
    (JSR, [ OperandImm off11 ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x4
      selBit  <- placeBits ln lineStr 11 11 0x1
      offBits <- placeBits ln lineStr 0 10 (fromIntegral off11)
      return $ opBits .|. selBit .|. offBits
    (JSRR, [ OperandRegId baser ]) -> do
      opBits    <- placeBits ln lineStr 12 15 0x4
      selBit    <- placeBits ln lineStr 11 11 0x0
      baserBits <- placeBits ln lineStr 6 8 (fromIntegral baser)
      return $ opBits .|. selBit .|. baserBits
    (LDB, [ OperandRegId dr
          , OperandRegId baser
          , OperandImm off6 ]) -> do
      opBits    <- placeBits ln lineStr 12 15 0x2
      drBits    <- placeBits ln lineStr 9 11 (fromIntegral dr)
      baserBits <- placeBits ln lineStr 6 8 (fromIntegral baser)
      offBits   <- placeBits ln lineStr 0 5 (fromIntegral off6)
      return $ opBits .|. drBits .|. baserBits .|. offBits
    (LDW, [ OperandRegId dr
          , OperandRegId baser
          , OperandImm off6 ]) -> do
      opBits    <- placeBits ln lineStr 12 15 0x6
      drBits    <- placeBits ln lineStr 9 11 (fromIntegral dr)
      baserBits <- placeBits ln lineStr 6 8 (fromIntegral baser)
      -- NOTE: We encode the offset directly, which means that during execution,
      -- the value in the assembly code gets implicitly doubled. This matches the
       -- ISA but is somewhat counterintuitive.
      offBits   <- placeBits ln lineStr 0 5 (fromIntegral off6)
      return $ opBits .|. drBits .|. baserBits .|. offBits
    (LEA, [ OperandRegId dr
          , OperandImm addr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0xE
      drBits  <- placeBits ln lineStr 9 11 (fromIntegral dr)
      offBits <- placeBits ln lineStr 0 8 (fromIntegral ((addr - (la + 2)) `ashiftR` 1))
      return $ opBits .|. drBits .|. offBits
    (NOT, [ OperandRegId dr
          , OperandRegId sr ]) -> do
      opBits  <- placeBits ln lineStr 12 15 0x9
      drBits  <- placeBits ln lineStr 9 11 (fromIntegral dr)
      srBits  <- placeBits ln lineStr 6 8 (fromIntegral sr)
      oneBits <- placeBits ln lineStr 0 5 0x3F
      return $ opBits .|. drBits .|. srBits .|. oneBits
    (RET, []) -> return 0xC1C0
    (RTI, []) -> return 0x8000
    (LSHF, [ OperandRegId dr
           , OperandRegId sr
           , OperandImm amount4 ]) -> do
      opBits      <- placeBits ln lineStr 12 15 0xD
      drBits      <- placeBits ln lineStr 9  11 (fromIntegral dr)
      srBits      <- placeBits ln lineStr 6  8  (fromIntegral sr)
      selBits     <- placeBits ln lineStr 4 5  0x0
      amount4Bits <- placeBits ln lineStr 0 3 (fromIntegral amount4)
      return $ opBits .|. drBits .|. srBits .|. selBits .|. amount4Bits
    (RSHFL, [ OperandRegId dr
            , OperandRegId sr
            , OperandImm amount4 ]) -> do
      opBits      <- placeBits ln lineStr 12 15 0xD
      drBits      <- placeBits ln lineStr 9  11 (fromIntegral dr)
      srBits      <- placeBits ln lineStr 6  8  (fromIntegral sr)
      selBits     <- placeBits ln lineStr 4 5  0x1
      amount4Bits <- placeBits ln lineStr 0 3 (fromIntegral amount4)
      return $ opBits .|. drBits .|. srBits .|. selBits .|. amount4Bits
    (RSHFA, [ OperandRegId dr
            , OperandRegId sr
            , OperandImm amount4 ]) -> do
      opBits      <- placeBits ln lineStr 12 15 0xD
      drBits      <- placeBits ln lineStr 9  11 (fromIntegral dr)
      srBits      <- placeBits ln lineStr 6  8  (fromIntegral sr)
      selBits     <- placeBits ln lineStr 4 5  0x3
      amount4Bits <- placeBits ln lineStr 0 3 (fromIntegral amount4)
      return $ opBits .|. drBits .|. srBits .|. selBits .|. amount4Bits
    (STB, [ OperandRegId sr
          , OperandRegId baser
          , OperandImm boffset6 ]) -> do
      opBits       <- placeBits ln lineStr 12 15 0x3
      srBits       <- placeBits ln lineStr 9  11 (fromIntegral sr)
      baserBits    <- placeBits ln lineStr 6 8 (fromIntegral baser)
      boffset6Bits <- placeBits ln lineStr 0 5 (fromIntegral boffset6)
      return $ opBits .|. srBits .|. baserBits .|. boffset6Bits
    (STW, [ OperandRegId sr
          , OperandRegId baser
          , OperandImm offset6 ]) -> do
      opBits      <- placeBits ln lineStr 12 15 0x3
      srBits      <- placeBits ln lineStr 9  11 (fromIntegral sr)
      baserBits   <- placeBits ln lineStr 6 8 (fromIntegral baser)
      offset6Bits <- placeBits ln lineStr 0 5 (fromIntegral (offset6 `ashiftR` 1))
      return $ opBits .|. srBits .|. baserBits .|. offset6Bits
    (TRAP, [ OperandImm trapvect8 ]) -> do
      opBits        <- placeBits ln lineStr 12 15 0xF
      trapvect8Bits <- placeBits ln lineStr 0 7 trapvect8
      return $ opBits .|. trapvect8Bits
    (XOR, [ OperandRegId dr
          , OperandRegId sr1
          , OperandRegId sr2 ] ) -> do
      opBits  <- placeBits ln lineStr 12 15 0x9
      drBits  <- placeBits ln lineStr 9  11 (fromIntegral dr)
      sr1Bits <- placeBits ln lineStr 6  8  (fromIntegral sr1)
      sr2Bits <- placeBits ln lineStr 0  2  (fromIntegral sr2)
      selBit  <- placeBits ln lineStr 5  5  0x0
      return $ opBits .|. drBits .|. sr1Bits .|. sr2Bits .|. selBit
    (XOR, [ OperandRegId dr
          , OperandRegId sr1
          , OperandImm imm5 ] ) -> do
      opBits  <- placeBits ln lineStr 12 15 0x9
      drBits  <- placeBits ln lineStr 9  11 (fromIntegral dr)
      sr1Bits <- placeBits ln lineStr 6  8  (fromIntegral sr1)
      immBits <- placeBits ln lineStr 0  4  (fromIntegral imm5)
      selBit  <- placeBits ln lineStr 5  5  0x1
      return $ opBits .|. drBits .|. sr1Bits .|. immBits .|. selBit
    _ -> Left (OperandTypeError ln lineStr)
assembleLine (Line (LineDataLiteral imm) _ _ _) = return imm

-- | Assemble a parsed program into a bytestring.
assembleProgram :: Program -> Either ParseException ByteString
assembleProgram prog = wordsToBS <$> (sequence $ assembleLine <$> prog)
