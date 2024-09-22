module Template.Golang.Compiler where

import Control.Monad.State (State, get, put, modify, runState)
import Control.Monad (foldM)

import qualified Data.ByteString as Bs
import Data.Either (isRight)
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, pack)
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import Conclusion (GenError (..))
import RunTime.Interpreter.Context (ConstantValue (..), FunctionDef (..), FunctionCode (ByteCode))
import RunTime.Interpreter.OpCodes (OpCode (..), opParCount)
import Text.Cannelle.Hugo.AST

type MainText = Bs.ByteString

data CompContext = CompContext {
    constants :: Mp.Map MainText (ConstantValue, Int32)
    , functions :: Mp.Map MainText (FunctionDef, Int32)
    , hasFailed :: Maybe GenError
    , labels :: Mp.Map Int32 Int32
    , bytecode :: [ OpCode ]
    , unitCounter :: Int32
    , spitFctID :: Int32
    , iterLabels :: [(Int32, Int32)]
    , currentFunctionDef :: [FunctionDef]
  }
  deriving Show

newtype CompError = CompError [(Int32, String)]
  deriving Show


data VarType =
  SimpleTypeVT SimpleType
  | MonadicTypeVT MainText [ VarType ]
  | VarTypeVT
  deriving Show


data SimpleType =
  IntST
  | FloatST
  | BoolST
  | StringST
  | StructST MainText
  deriving Show


type CompileResult = State CompContext (Either CompError ())


initCompContext :: CompContext
initCompContext = CompContext {
  constants = Mp.empty
  , functions = Mp.empty
  , hasFailed = Nothing
  , labels = Mp.empty
  , bytecode = []
  , unitCounter = 0
  , spitFctID = 0
  , iterLabels = []
  , currentFunctionDef = []
}


initFunctionDef :: MainText -> FunctionDef
initFunctionDef aLabel = FunctionDef {
      name = ""
    , args = [ ]
    , heapDef = 0
    , body = ByteCode V.empty
  }


concatErrors :: [CompError] -> CompError
concatErrors = CompError . concatMap (\(CompError errs) -> errs)

emitOp :: OpCode -> CompileResult
emitOp instr = do
  modify $ \s -> s { bytecode = s.bytecode <> [instr] }
  pure $ Right ()


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
    Nothing -> do
      modify $ \s -> s { hasFailed = Just . SimpleMsg . pack $ "Label " <> show label <> "not found." }
      pure $ Right ()
    Just aPos -> do
      modify $ \s ->
        let
          (before, after) = splitAt (fromIntegral aPos) s.bytecode
          afterSize = sum . map (\i -> 1 + opParCount i) $ after
        in
        s { bytecode = before <> [JUMP_ABS $ fromIntegral afterSize] <> after }
      pure $ Right ()


addStringConstant :: MainText -> State CompContext Int32
addStringConstant newConst =
  addTypedConstant (StringCte newConst) $ Cr.hash newConst

addVerbatimConstant :: MainText -> State CompContext Int32
addVerbatimConstant newConst =
  addTypedConstant (VerbatimCte newConst) $ Cr.hash newConst


addTypedConstant :: ConstantValue -> MainText -> State CompContext Int32
addTypedConstant newConst md5Hash = do
    s <- get
    let
      existing = Mp.lookup md5Hash s.constants
    case existing of
      Just (value, index) -> pure index
      Nothing ->
        let
          index = fromIntegral $ Mp.size s.constants
        in do
        put s { constants = Mp.insert md5Hash (newConst, index) s.constants }
        pure index


-- TODO: implement.
registerVariable :: Variable -> VarType -> State CompContext Int32
registerVariable (Variable varKind label) varType = pure 1
-- VarKind = LocalK ($aVar) | MethodK (.aMethod) | LocalMethodK ($.aMethod)

-- TODO: implement.
dereferVariable :: Variable -> State CompContext (Maybe Int32)
dereferVariable label = pure Nothing

-- TODO: implement; it returns the index of a new local variable that will hold the localContext within the
-- with-block. These can work as a stack.
registerWithContext :: VarType -> State CompContext Int32
registerWithContext varType = pure 1

setIterationLabel :: (Int32, Int32) -> CompileResult
setIterationLabel (iterLabel, endLabel) = do
  modify $ \ctx -> ctx { iterLabels = (iterLabel, endLabel) : ctx.iterLabels }
  pure $ Right ()

clearIterationLabel :: CompileResult
clearIterationLabel = do
  modify $ \ctx -> ctx { iterLabels = tail ctx.iterLabels }
  pure $ Right ()

getIterationLabel :: State CompContext (Maybe (Int32, Int32))
getIterationLabel = do
  ctx <- get
  case ctx.iterLabels of
    [] -> pure Nothing
    h : _ -> pure $ Just h

-- TODO: proper implementation.
registerFunction :: MainText -> State CompContext (Maybe Int32)
registerFunction funcName = do
  ctx <- get
  case Mp.lookup funcName ctx.functions of
    Just (funcDef, funcID) -> pure Nothing
    Nothing -> do
      let
        functionID = fromIntegral $ Mp.size ctx.functions
      put ctx { functions = Mp.insert funcName (initFunctionDef funcName, functionID) ctx.functions }
      pure $ Just functionID

getFunctionID :: MainText -> State CompContext (Maybe Int32)
getFunctionID funcName = do
  ctx <- get
  case Mp.lookup funcName ctx.functions of
    Just (_, funcID) -> pure $ Just funcID
    Nothing -> pure Nothing


startFunctionContext :: MainText -> CompileResult
startFunctionContext label = do
  ctx <- get
  -- TODO: check that the label exists in the functions map.
  let
    (funcDef, funcID) = ctx.functions Mp.! label
  put ctx { currentFunctionDef = funcDef : ctx.currentFunctionDef }
  pure $ Right ()


endFunctionContext :: CompileResult
endFunctionContext = do
  ctx <- get
  put ctx { currentFunctionDef = tail ctx.currentFunctionDef }
  pure $ Right ()

-- TODO: implement.
registerBlock :: MainText -> State CompContext Int32
registerBlock label = pure 0

-- TODO: implement.
setFunctionContext :: Int32 -> CompileResult
setFunctionContext funcID = pure $ Right ()

-- TODO: implement.
getInternalTemplate :: MainText -> State CompContext Int32
getInternalTemplate label = pure 0

-- TODO: implement.
getExternalTemplate :: MainText -> State CompContext Int32
getExternalTemplate label = pure 0


compileStatements :: [Statement] -> Either CompError CompContext
compileStatements stmts =
  -- TODO: find out how to detect errors and pass on to caller.
  let
    (rezA, finalState) = runState (mapM compileStmt stmts) initCompContext  
  in
  case rezA of
    [] -> Right finalState
    _ ->
      let
        errors = foldr (\r accum-> case r of Right _ -> accum; Left err -> err : accum) [] rezA
      in
      case errors of
        [] -> Right finalState
        _ -> Left $ concatErrors errors


compileStmt :: Statement -> CompileResult
compileStmt (VarAssignST assgnKind var expr) = do
  eiVarID <- case assgnKind of
    DefinitionK -> do
      eiVarID <- dereferVariable var
      case eiVarID of
        -- TODO: carry the line number into the statements so they can show up in error messages.
        Just varID -> pure . Left $ CompError [(0, "Variable already defined: " <> show var)]
        -- TODO: extract type expected from the variable.
        Nothing -> Right <$> registerVariable var (SimpleTypeVT IntST)
  case eiVarID of
    Left err -> pure $ Left err
    Right varID -> do
      compileExpression expr
      emitOp $ SET_VAR varID


compileStmt (VerbatimST text) = do
  cteID <- addVerbatimConstant text
  emitOp $ PUSH_CONST cteID
  ctx <- get
  emitOp $ REDUCE ctx.spitFctID 1


compileStmt (IfST condExpr thenStmts elseStmts) = do
  elseLabel <- newLabel
  endLabel <- newLabel
  compileExpression condExpr
  emitOp CMP_BOOL_IMM
  emitOp $ JUMP_FALSE elseLabel
  compileStmt thenStmts
  emitOp $ JUMP_ABS endLabel
  resolveLabel elseLabel
  compileStmt elseStmts
  resolveLabel endLabel


compileStmt (RangeST mbVars expr thenStmt elseStmt) = do
  mbValIDs <- case mbVars of
    Just (RangeVars valVar mbIdxVar) -> do
      -- TODO: extract type expected from the variable.
      valID <- registerVariable valVar (SimpleTypeVT IntST)
      mbIdxID <- case mbIdxVar of
        Nothing -> pure Nothing
        Just aVar -> Just <$> registerVariable aVar (SimpleTypeVT IntST)
      pure $ Just (valID, mbIdxID)
    Nothing -> pure Nothing
  iterLabel <- newLabel
  elseLabel <- newLabel
  endLabel <- newLabel
  -- TODO: figure out how to handle the iterator's implicit looping index variable.
  compileIterator iterLabel mbValIDs expr
  emitOp CMP_BOOL_IMM
  emitOp $ JUMP_FALSE elseLabel
  -- TODO: pass the iterLabel and endLabel to the compileStmt so a 'break' or 'continue' can be associated to the
  --  right position in the bytecode.
  setIterationLabel (iterLabel, endLabel)
  compileStmt thenStmt
  emitOp $ JUMP_ABS iterLabel
  resolveLabel elseLabel
  compileStmt elseStmt
  resolveLabel endLabel
  clearIterationLabel


compileStmt (WithST expr thenStmt elseStmt) = do
  -- TODO: extract type expected from the expr and use it to specialise the context variable type.
  withCtxtID <- registerWithContext (SimpleTypeVT $ StructST "Anything")
  elseLabel <- newLabel
  endLabel <- newLabel
  compileExpression expr
  emitOp DUP_1
  emitOp CMP_BOOL_IMM
  emitOp $ JUMP_FALSE elseLabel
  emitOp $ SET_VAR withCtxtID
  compileStmt thenStmt
  emitOp $ JUMP_ABS endLabel
  emitOp $ SET_VAR withCtxtID
  resolveLabel elseLabel
  compileStmt elseStmt
  resolveLabel endLabel


compileStmt (ReturnST expr) = do
  compileExpression expr
  -- TODO: put the right number of values to return.
  emitOp $ RETURN 0


compileStmt ContinueST = do
  mbIterLabels <- getIterationLabel
  case mbIterLabels of
    Just (iterLabel, endLabel) -> emitOp $ JUMP_ABS iterLabel
    Nothing -> pure $ Left $ CompError [(0, "No active loop to continue.")]


compileStmt BreakST = do
  mbIterLabels <- getIterationLabel
  case mbIterLabels of
    Just (iterLabel, endLabel) -> emitOp $ JUMP_ABS endLabel


compileStmt (ExpressionST expr) = do
  ctx <- get
  compileExpression expr
  emitOp FORCE_TO_STRING
  emitOp $ REDUCE ctx.spitFctID 1


compileStmt (DefineST label body) = do
  mbFunctionID <- registerFunction label
  case mbFunctionID of
    Just functionID -> do
      startFunctionContext label
      compileStmt body
      -- TODO: check if the define block returns a value.
      -- TODO: put the right number of values to return.
      emitOp $ RETURN 0
      endFunctionContext
    Nothing -> pure $ Left $ CompError [(0, "Function already defined: " <> show label)]


compileStmt (BlockST label contextExpr stmts) = do
  blockID <- registerBlock label
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  compileExpression contextExpr
  emitOp $ SET_VAR localCtx
  emitOp $ REDUCE blockID 1
  setFunctionContext blockID
  compileStmt stmts
  -- TODO: put the right number of values to return.
  emitOp $ RETURN 0
  endFunctionContext

compileStmt (IncludeST label expr) = do
  templateID <- getInternalTemplate label
  compileExpression expr
  emitOp $ REDUCE templateID 1


compileStmt (PartialST label expr) = do
  templateID <- getExternalTemplate label
  compileExpression expr
  emitOp $ REDUCE templateID 1


compileStmt (ListST stmts) = do
  rezA <- mapM compileStmt stmts
  case rezA of
    [] -> pure $ Right ()
    _ ->
      let
        errors = foldr (\r accum-> case r of Right _ -> accum; Left err -> err : accum) [] rezA
      in
      case errors of
        [] -> pure $ Right ()
        _ -> pure . Left $ concatErrors errors


compileStmt NoOpST = pure $ Right ()


compileIterator :: Int32 -> Maybe (Int32, Maybe Int32) -> Expression -> CompileResult
compileIterator iterLabel Nothing expr = do
  resolveLabel iterLabel
  compileExpression expr

compileIterator iterLabel (Just (varID, Nothing)) expr = do
  resolveLabel iterLabel
  compileExpression expr
  emitOp DUP_1
  emitOp $ SET_VAR varID

compileIterator iterLabel (Just (varID, Just idxVarID)) expr = do
  emitOp $ SET_VAR_IM1 idxVarID
  resolveLabel iterLabel
  compileExpression expr
  emitOp DUP_1
  emitOp $ SET_VAR varID
  emitOp $ GET_VAR idxVarID
  emitOp IINC_1
  emitOp $ SET_VAR idxVarID


compileExpression :: Expression -> CompileResult
compileExpression (ExprLiteral lit) = case lit of
    LitString s -> do
      cteID <- addStringConstant s
      emitOp $ PUSH_CONST cteID
    LitNumber n -> emitOp $ PUSH_DOUBLE_IMM n
    LitBool b -> emitOp $ PUSH_BOOL_IMM b


compileExpression (ExprVariable var) = do
  -- TODO: extract type expected from the variable.
  varID <- registerVariable var (SimpleTypeVT IntST)
  emitOp $ GET_VAR varID


compileExpression ExprCurrentContext = do
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  emitOp $ GET_VAR localCtx


compileExpression ExprParentContext = do
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  emitOp $ GET_VAR localCtx
  parentLabelID <- addStringConstant "$parentCtx"
  emitOp $ PUSH_CONST parentLabelID
  emitOp $ GET_FIELD 


compileExpression (ExprMethodAccess fields values) = do
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  emitOp $ GET_VAR localCtx
  -- TODO: implement properly.
  mapM_ (\(Variable varKind varName) -> do
      lID <- addStringConstant varName
      emitOp $ PUSH_CONST lID
      emitOp $ GET_FIELD
    ) fields
  mapM_ compileExpression values
  emitOp $ CALL_METHOD (fromIntegral $ length fields)


compileExpression (ExprFunctionCall funcName args) = do
  mbFunctionID <- getFunctionID funcName
  case mbFunctionID of
    Just functionID -> do
      mapM_ compileExpression args
      emitOp $ REDUCE functionID (fromIntegral $ length args)
    Nothing -> pure $ Left $ CompError [(0, "Function not found: " <> show funcName)]


compileExpression (ExprPipeline expr functions) = do
  compileExpression expr
  rezB <- mapM (\(FunctionApplication funcName args) -> do
      mbFunctionID <- getFunctionID funcName
      case mbFunctionID of
        Nothing -> pure $ Left $ CompError [(0, "Function not found: " <> show funcName)]
        Just functionID -> do
          -- TODO: check for errors in rezA.
          rezA <- mapM_ compileExpression args
          emitOp $ REDUCE functionID (1 + fromIntegral (length args))
    ) functions
  -- TODO: check for errors in rezB.
  pure $ Right ()
  