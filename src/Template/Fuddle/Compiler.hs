{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Template.Fuddle.Compiler where

import Control.Monad (foldM, when)
import Control.Monad.State (State, get, put, runState, modify)

import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Vector as Vc
import qualified Data.Map as Mp
import qualified Crypto.Hash.MD5 as Cr
import qualified Data.Text.Encoding as TE

import Conclusion (GenError (..))
import RunTime.Interpreter.OpCodes (OpCode (..), toInstr, opParCount)
import RunTime.Interpreter.Context (VMModule (..), FunctionDef (..), ConstantValue (..), ModuledDefinition (..), FunctionCode (..))
import Template.Fuddle.BAst


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
    , labels :: Mp.Map Int32 Int32
    , bytecode :: [ OpCode ]
    , unitCounter :: Int32
    , unitStack :: [ CompileUnit]
    -- Shortcut to the all-omnipresent spit function.
    , spitFunction :: Int32
  }

data BindDefinition = BindDefinition QualifiedIdent FuddleType

data FuddleType =
  BoolFT | CharFT | IntFT | FloatFT | DoubleFT | StringFT
  | ArrayFT FuddleType
  | TupleFT [ FuddleType ]
  -- a record with named fields:
  | StructureFT (Mp.Map Text FuddleType)
  -- array of parameters, return type:
  | FunctionFT [ FuddleType ] FuddleType


data CompileUnit = CompileUnit {
    name :: Text
    , unitID :: Int
    , arguments :: [ BindDefinition ]
    , localVars :: [ BindDefinition ]
    , bcode :: [ OpCode ]
  }


initContext preludeMods = CompContext {
  idents = Mp.empty
  , imports = []
  , constants = Mp.empty
  , constantMap = Mp.empty
  , bindings = Mp.fromList [ ("$main", FunctionDef "$main" [] 0 (ByteCode Vc.empty)) ]
  , referredIdentifiers = Mp.empty
  , modules = preludeMods
  , hasFailed = Nothing
  , labels = Mp.empty
  , bytecode = []
  , unitCounter = 0
  , unitStack = []
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
{-
  - result: VMModule, with the bytecode, the constants, the function definitions, the required modules/functions.
-}
compileAstTree :: NodeAst -> Either GenError VMModule
compileAstTree nTree =
  -- putStrLn $ "@[compileAst] ast: " ++ show ast
  case nTree of
    AstLogic (StmtAst (SeqST []) children) ->
      let
        -- TODO: pass the pre-loaded prelude modules.
        context = initContext Mp.empty
        (retValue, rezContext) = runState (compileTree nTree) context
      in
      case rezContext.hasFailed of
        Just err -> Left err
        Nothing ->
          let
            tmpMain = FunctionDef "$main" [] 0 (ByteCode $ Vc.fromList $ concatMap toInstr rezContext.bytecode)
            keyVals = sortBy (\(ak, av) (bk, bv) -> if av == bv then EQ else if av < bv then LT else GT ) $ Mp.toList rezContext.constantMap
          in
          Right $ VMModule {
              functions = [ tmpMain ]
              , constants = Vc.fromList $ map (\(k, v) -> rezContext.constants Mp.! k) keyVals
              , externModules = modules rezContext
            }
    _ -> Left $ SimpleMsg "Unexpected root AST node for compilation."


emitOp :: OpCode -> CompileResult
emitOp instr = modify $ \s -> s { bytecode = s.bytecode <> [instr] }

newLabel :: State CompContext Int32
newLabel = do
    s <- get
    let
      labelID = fromIntegral $ Mp.size s.labels
    put s { labels = Mp.insert labelID (fromIntegral $ length s.bytecode) s.labels }
    return labelID


resolveLabel :: Int32 -> CompileResult
resolveLabel label = do
  s <- get
  case Mp.lookup label s.labels of
    Nothing -> modify $ \s -> s { hasFailed = Just . SimpleMsg . pack $ "Label " <> show label <> "not found." }
    Just aPos -> modify $ \s ->
      let
        (before, after) = splitAt (fromIntegral aPos) s.bytecode
        afterSize = sum . map (\i -> 1 + opParCount i) $ after
      in
      s { bytecode = before <> [JUMP_ABS $ fromIntegral afterSize] <> after }


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


compileTree :: NodeAst -> CompileResult
compileTree node = do
  compileNode node
  emitOp HALT


compileNode :: NodeAst -> CompileResult
compileNode node=
  case node of
    AstLogic (StmtAst stmt children) ->
      compileStmt stmt children
    CloneText someText -> do
      s <- get
      newIndex <- addStringConstant someText
      emitOp $ PUSH_CONST newIndex
      emitOp $ REDUCE s.spitFunction 1
      pure ()


compileStmt :: StatementFd -> [NodeAst] -> CompileResult
compileStmt ast children =
  case ast of
    SeqST stmts -> do
      mapM_ (`compileStmt` []) stmts
      mapM_ compileNode children
    ElseIfShortST isElse cond args ->
      {- TODO:
        - if isElse, the not-true branch label resolution should lead to this bytecode position,
          the NodeAst parser should have made sure that the previous if is well-formed. 
        - compile the condition expression,
        - create a new label for the not-true branch,
        - create a frame for the local children compilation with the arguments as local variables,
        - compile the children,
        - resolve the label for the not-true branch.        
      -}
      if null children then
        pure ()
      else do
        compileExpr cond
        emitOp CMP_BOOL_IMM
        emitOp $ JUMP_TRUE 2
        nextLabel <- newLabel
        mapM_ compileNode children
        resolveLabel nextLabel
    BlockEndST ->
      --TODO.
      pure ()
    IfElseNilSS cond args ->
      if null children then
        pure ()
      else do
        compileExpr cond
        -- TODO: manage the label system so it can take a type-of-jump and later do the distance resolution using that
        -- instead of the double-jump approach proposed by Copilot.
        -- Also take care of args and children compilation block.
        emitOp CMP_BOOL_IMM
        emitOp $ JUMP_TRUE 2    -- move PC over the jump+distance if false, ie execute the block.
        nextLabel <- newLabel   -- skip the block.
        mapM_ compileNode children
        resolveLabel nextLabel  -- resolve the distance and insert the jump instruction at label position.
    BindOneST (ident, params) expr ->
      {- TODO:
        - create a new function context, set the name as ident, check for clash,
          set the parameter arity + add the param names to the local variable list,
        - push the function context to the function stack,
        - compile the expression,
        - pop the function context from the function stack, add the function to the parent context.
        !! if arity == 0, it's equivalent to a variable definition... handle the same way?
      -}
      pure ()
    ExpressionST expr -> do
      s <- get
      compileExpr expr
      emitOp $ REDUCE s.spitFunction 1
    -- ImportST isQualified qualIdent mbQualIdents: should not happen here, it's used before to build reference for modules in local space.
    _ -> pure ()


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
compileReduction qualIdent exprArray = do
  {- TODO:
  - find the identifier in the context,
    - if not found, put it in the unresolved list,
  - compile each expressions, putting the result on the stack,
  - insert a function call op to the identifier address: it's a pointer to the function list (both identified and unindentified).
  !! inline functions with arity 0 -> variable access.
  -}
  s <- get
  functionID <- case qualIdent of
      "$spit" :| [] -> pure s.spitFunction
      _ -> do
        -- TODO: find the function in the context
        pure 1
  mapM_ compileExpr exprArray
  emitOp $ REDUCE functionID (fromIntegral . length $ exprArray)

