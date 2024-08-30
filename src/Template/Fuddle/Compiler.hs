{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Template.Fuddle.Compiler where

import Control.Monad (foldM)
import Control.Monad.State (State, get, put, runState, modify)

import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import qualified Data.Vector as Vc
import qualified Data.Map as Mp
import qualified Crypto.Hash.MD5 as Cr
import qualified Data.Text.Encoding as TE

import Conclusion (GenError (..))
import RunTime.Interpreter.OpCodes (OpCode (..))
import Template.Fuddle.BAst


type VMCode = Vc.Vector Int

data FunctionDef = FunctionDef {
    funcName :: Text
    , funcArgs :: [ Text ]
    , funcBody :: FunctionCode
  }


data FunctionCode =
  NativeCode
  | ByteCode VMCode


data ModuledDefinition = ModuledDefinition {
    modName :: Text
    , modBody :: Mp.Map Text FunctionDef
  }


data ReferenceDetails =
  UnknownIdent Text
  | ExternalRef Text
  | InternalRef Int


data CompContext = CompContext {
    idents :: Mp.Map Text Int
    , imports :: [ String ]
    , constants :: Mp.Map ByteString ConstantValue
    , constantMap :: Mp.Map ByteString Int32
    , bindings :: Mp.Map Text FunctionDef
    , referredIdentifiers :: Mp.Map Text ReferenceDetails
    , modules :: Mp.Map Text ModuledDefinition
    , hasFailed :: Maybe GenError
    , labelID :: Int
    , bytecode :: [ OpCode ]
    -- Shortcut to the all-omnipresent spit function.
    , spitFunction :: Int32
  }


data ConstantValue =
  StringCte ByteString
  | IntCte Int
  | FloatCte Float
  | DoubleCte Double
  | ArrayCte [ ConstantValue ]


initContext preludeMods = CompContext {
  idents = Mp.empty
  , imports = []
  , constants = Mp.empty
  , constantMap = Mp.empty
  , bindings = Mp.fromList [ ("main", FunctionDef "main" [] (ByteCode Vc.empty)) ]
  , referredIdentifiers = Mp.empty
  , modules = preludeMods
  , hasFailed = Nothing
  , labelID = 0
  , bytecode = []
  , spitFunction = 0
}


type CompileResult = State CompContext ()

{-
    - Once the AST node tree is created:
      - resolve all identifiers,
      - compile the AST node tree:
        - CloneText: add opcodes to spit the text,
        - Logic/Stmt:
          - Seq: compile each statement,
          - ElseIfShort: compile the condition expr, jmp on false to next stmt, compile the stmt, emit next-stmt label.
          - BlockEnd: emit next-stmt label.
          - IfElseNil: compile the condition expr, jmp on false to next stmt, compile the stmt, emit next-stmt label.
          - Import: skip (already processed before compilation).
          - BindOne: push the function compilation context, create new context with args, compile the expression
              , pop the function context, add function to parent context.
          - Let: compile each expression, add the bindings to the context, compile the last expression.
          - ExpressionST: compile the expression, emit a reduction op to the spit function.
        - Logic/Expr:
          - Literal: add the literal value to the stack.
          - Paren: compile the expression.
          - Array: compile each expression, unstack to the array memory location, push the array location to stack.
            ? How to implement a range expression ?
          - Unary: compile the expression, emit unary operator (pop arg, compute, push result).
          - Binary: compile the two expressions and apply the binary operator (pop 2 args, compute, push result).
          - Reduction: compile the expressions, emit a reduction op to function ID with # of expressions results on stack.
      - add the VM code to the global context.
      - consolidate constants, local vars, function definitions.
-}

-- TODO: add a parameter for passing the pre-loaded prelude modules.
compileAstTree :: NodeAst -> Either GenError VMCode
compileAstTree nTree =
  -- putStrLn $ "@[compileAst] ast: " ++ show ast
  case nTree of
    AstLogic (StmtAst (SeqST []) children) ->     
      let
        -- TODO: pass the pre-loaded prelude modules.
        context = initContext Mp.empty
        (retValue, rezContext) = runState (compileNode nTree) context
      in
      case rezContext.hasFailed of
        Just err -> Left err
        Nothing ->
          Right . Vc.fromList $ map fromEnum rezContext.bytecode
    _ -> Left $ SimpleMsg "Unexpected root AST node for compilation."


emitOp :: OpCode -> CompileResult
emitOp instr = modify $ \s -> s { bytecode = s.bytecode <> [instr] }

newLabel :: State CompContext Int
newLabel = do
    s <- get
    put s { labelID = s.labelID + 1 }
    return s.labelID


resolveLabel :: Int32 -> CompileResult
resolveLabel label = modify $ \s ->
    let
      (before, after) = splitAt (fromIntegral label) s.bytecode
    in
    s { bytecode = before ++ [JUMP_ABS (fromIntegral . length $ after)] ++ after }


addStringConstant :: ByteString -> State CompContext Int32
addStringConstant newConst = do
    s <- get
    let
      md5Hash = Cr.hash newConst
      existing = Mp.lookup md5Hash s.constants
    case existing of
      Just _ ->
        {- Sanity check on hash map: right now fix if missing entry, but could be worth raising an error instead... -}
        case Mp.lookup md5Hash s.constantMap of
          Just index -> pure index
          Nothing ->
            let
              index = fromIntegral $ Mp.size s.constantMap
            in do
            put s { constantMap = Mp.insert md5Hash index s.constantMap }
            pure index
      Nothing ->
        let
          index = fromIntegral $ Mp.size s.constantMap
        in do
        put s { constantMap = Mp.insert md5Hash index s.constantMap, constants = Mp.insert md5Hash (StringCte newConst) s.constants }
        pure index


compileNode :: NodeAst -> CompileResult
compileNode node=
  case node of
    AstLogic (StmtAst stmt children) -> do
      compileStmt stmt
      s <- get
      case s.hasFailed of
        Just _ -> pure ()
        Nothing -> mapM_ compileNode children
    CloneText someText -> do
      s <- get
      newIndex <- addStringConstant someText
      emitOp $ PUSH_INT_IMM $ fromIntegral newIndex
      emitOp $ REDUCE s.spitFunction 1
      pure ()


compileStmt :: StatementFd -> CompileResult
compileStmt ast =
  case ast of
    SeqST stmts -> mapM_ compileStmt stmts
    ElseIfShortST isElse cond args -> pure ()
    BlockEndST -> pure ()
    IfElseNilSS cond args -> pure ()
    ImportST isQualified qualIdent mbQualIdents ->
      {- Inject referred modules in local space. -}
      pure ()
    BindOneST identParams expr -> pure ()
    ExpressionST expr -> compileExpr expr


{- Expressions: -}
compileExpr :: Expression -> CompileResult
compileExpr expr =
  case expr of
    LiteralExpr literal -> compileLiteral literal
    ParenExpr expr -> compileExpr expr
    ArrayExpr exprArray -> compileExprArray exprArray
    UnaryExpr oper expr -> compileUniOper oper expr
    BinOpExpr oper exprA exprB -> compileBinOper oper exprA exprB
    ReductionExpr qualIdent exprArray -> compileReduction qualIdent exprArray


compileLiteral :: LiteralValue -> CompileResult
compileLiteral lit =
  case lit of
    NumeralValue lit -> compileNumeral lit
    BoolValue lit -> compileBool lit
    CharValue lit -> compileChar lit
    StringValue lit -> compileString lit
    ArrayValue litArray -> compileLitArray litArray

{- Literals: -}
compileNumeral :: Int -> CompileResult
compileNumeral lit = do
  -- TODO: type analysis to select the right kind of numeral.
  emitOp $ PUSH_INT_IMM $ fromIntegral lit

compileBool :: Bool -> CompileResult
compileBool lit =
  emitOp $ PUSH_BOOL_IMM lit

compileChar :: Char -> CompileResult
compileChar lit =
  emitOp $ PUSH_CHAR_IMM lit

compileString :: Text -> CompileResult
compileString lit = do
  strID <- addStringConstant $ TE.encodeUtf8 lit
  emitOp $ PUSH_CONST strID

compileLitArray :: [ LiteralValue ] -> CompileResult
compileLitArray lit = 
  -- TODO: how to store an array of literals ?
  pure ()

{- Array: -}
compileExprArray :: [ Expression ] -> CompileResult
compileExprArray exprArray =
  let
    arrayID = 0  -- get new array storage ID.
  in
  mapM_ compileExpr exprArray
  -- TODO: how to store an array of literals ?
  -- push the values to array storage, push the array ID to stack.


{- Operations: -}
compileUniOper :: UnaryOp -> Expression -> CompileResult
compileUniOper oper exprA = do
  compileExpr exprA
  case oper of
    -- TODO: type analysis to select the right kind of negation.
    NegateOP -> emitOp INEGATE
    NotOP -> emitOp BNOT
    BitNotOP -> emitOp BNOT

compileBinOper :: BinaryOp -> Expression -> Expression -> CompileResult
compileBinOper oper exprA exprB = do
  compileExpr exprA
  compileExpr exprB
  -- TODO: type analysis to select the right kind of bin op.
  case oper of
    AddOP -> emitOp IADD
    SubstractOP -> emitOp ISUB
    MultiplyOP -> emitOp IMUL
    DivideOP -> emitOp IDIV
    ModuloOP -> emitOp IMOD
    BitXorOP -> emitOp BXOR
    BitOrOP -> emitOp BOR
    BitShiftLeftOP -> emitOp ISHL
    BitShiftRightOP -> emitOp ISHR
    OrOP -> emitOp BOR
    AndOP -> emitOp BAND
    EqOP -> emitOp CMP_INT_IMM
    NeOP -> emitOp CMP_INT_IMM
    LtOP -> emitOp CMP_INT_IMM
    LeOP -> emitOp CMP_INT_IMM
    GeOP -> emitOp CMP_INT_IMM
    GtOP -> emitOp CMP_INT_IMM
    ConcatOP -> emitOp ARR_CONCAT
    CarAddOP -> emitOp ARR_ADD
    -- TODO: array ops (concat, car-add).

{- Reduction: an identifier that is the main definition, and additional expressions that are the parameters to apply to the identifier -}
compileReduction :: QualifiedIdent -> [ Expression ] -> CompileResult
compileReduction qualIdent exprArray =
  {-
    - find the identifier in the context,
      - if not found, put it in the unresolved list,
    - compile each expressions, putting the result on the stack,
    - insert a function call op to the identifier address: it's a pointer to the function list (both identified and unindentified).
  -}
  pure ()

