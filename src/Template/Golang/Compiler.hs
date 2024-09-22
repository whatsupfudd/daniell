module Template.Golang.Compiler where

import Control.Monad.State (State, get, put, modify, runState)
import Control.Monad (foldM)

import qualified Data.ByteString as Bs
import Data.Either (isRight)
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as Mp
import Data.Maybe (maybe, fromJust)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import Conclusion (GenError (..))
import RunTime.Interpreter.Context (ConstantValue (..), FunctionDef (..), FunctionCode (..))
import RunTime.Interpreter.OpCodes (OpCode (..), opParCount, toInstr)
import Text.Cannelle.Hugo.AST


type MainText = Bs.ByteString

data CompContext = CompContext {
    constants :: Mp.Map MainText (ConstantValue, Int32)
    , functions :: Mp.Map MainText (FunctionDef, Int32)
    , hasFailed :: Maybe GenError
    , labels :: Mp.Map Int32 Int32
    , unitCounter :: Int32
    , spitFctID :: Int32
    , iterLabels :: [(Int32, Int32)]
    , curFctDef :: [ CompFunction ]
  }

instance Show CompContext where
  show c = "CompContext {\n    constants = ["
      <> concatMap (\cte -> "\n      , " <> show cte) (L.sortOn snd $ Mp.elems c.constants)
      <> "\n]  , functions = [" <> concatMap (\c -> "\n      , " <> show c) c.functions
      <> "\n]  , hasFailed = " <> show c.hasFailed
      <> "\n  , labels = " <> show c.labels <> ", unitCounter = " <> show c.unitCounter <> ", spitFctID = " <> show c.spitFctID <> ", iterLabels = " <> show c.iterLabels <> ", curFctDef = " <> show c.curFctDef <> "\n}"

data CompFunction = CompFunction {
    name :: MainText
    , args :: [ (MainText, VarType) ]
    , heapDef :: Int32
    , bytecode :: V.Vector OpCode
  }
  deriving Show


newtype CompError = CompError [(Int32, String)]
  deriving Show


data VarType =
  SimpleTypeVT SimpleType
  | MonadicTypeVT MainText [ VarType ]    -- Eg [ Int ], Maybe String, etc.
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


initCompContext :: MainText ->CompContext
initCompContext funcLabel = CompContext {
  constants = Mp.empty
  , functions = Mp.empty
  , hasFailed = Nothing
  , labels = Mp.empty
  , unitCounter = 0
  , spitFctID = 0
  , iterLabels = []
  , curFctDef = [initCompFunction funcLabel]
}


initCompFunction :: MainText -> CompFunction
initCompFunction aLabel = CompFunction {
      name = aLabel
    , args = []
    , heapDef = 0
    , bytecode = V.empty
  }


concatErrors :: [Either CompError a] -> Maybe CompError
concatErrors = foldl (\accum eiErr -> case eiErr of
    Left err@(CompError nList) -> case accum of
        Nothing -> Just err
        Just (CompError accumList) -> Just $ CompError (accumList <> nList)
    _ -> accum
  ) Nothing


emitOp :: OpCode -> CompileResult
emitOp instr = do
  ctx <- get
  case ctx.curFctDef of
    [] -> pure . Left $ CompError [(0, "No function available for emitting op code.")]
    fctDef : tDefs ->
      let
        newS = fctDef { bytecode = fctDef.bytecode <> V.fromList [instr] }
      in do
      put ctx { curFctDef = newS : tDefs }
      pure $ Right ()


newLabel :: State CompContext (Either CompError Int32)
newLabel = do
    ctx <- get
    let
      labelID = fromIntegral $ Mp.size ctx.labels
    case ctx.curFctDef of
      [] -> pure $ Left $ CompError [(0, "No function available for tagging new label.")]
      h : _ -> do
        put ctx { labels = Mp.insert labelID (fromIntegral $ V.length h.bytecode) ctx.labels }
        pure $ Right labelID


resolveLabel :: Int32 -> CompileResult
resolveLabel labelID = do
  ctx <- get
  case Mp.lookup labelID ctx.labels of
    Nothing ->
      let
        errMsg = "@[resolveLabel] label " <> show labelID <> " not found."
      in do
      put ctx { hasFailed = Just . SimpleMsg . pack $ errMsg }
      pure . Left $ CompError [(0, errMsg)]
    Just aPos -> 
      case ctx.curFctDef of
        [] -> pure $ Left $ CompError [(0, "@[resolveLabel] no function available for emitting op code at label " <> show labelID)]
        fctDef : tDefs ->
          let
            (before, after) = V.splitAt (fromIntegral aPos) fctDef.bytecode
            afterSize = sum . V.map (\i -> 1 + opParCount i) $ after
            newFctDef = fctDef { bytecode = before <> V.fromList [JUMP_ABS $ fromIntegral afterSize] <> after }
          in do
          put ctx { curFctDef = newFctDef : tDefs, labels = Mp.delete labelID ctx.labels }
          pure $ Right ()


addStringConstant :: MainText -> State CompContext Int32
addStringConstant newConst =
  addTypedConstant (StringCte newConst) $ Cr.hash newConst

addVerbatimConstant :: MainText -> State CompContext Int32
addVerbatimConstant newConst =
  addTypedConstant (VerbatimCte newConst) $ Cr.hash newConst


addTypedConstant :: ConstantValue -> MainText -> State CompContext Int32
addTypedConstant newConst md5Hash = do
    ctx <- get
    let
      existing = Mp.lookup md5Hash ctx.constants
    case existing of
      Just (value, index) -> pure index
      Nothing ->
        let
          index = fromIntegral $ Mp.size ctx.constants
        in do
        put ctx { constants = Mp.insert md5Hash (newConst, index) ctx.constants }
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
      put ctx { functions = Mp.insert funcName (FunctionDef (T.decodeUtf8 funcName) [] 0 NativeCode, functionID) ctx.functions }
      pure $ Just functionID

getFunctionID :: MainText -> State CompContext (Maybe Int32)
getFunctionID funcName = do
  ctx <- get
  case Mp.lookup funcName ctx.functions of
    Just (_, funcID) -> pure $ Just funcID
    Nothing -> pure Nothing


startFunctionComp :: MainText -> CompileResult
startFunctionComp label = do
  ctx <- get
  -- TODO: check that the label exists in the functions map.
  put ctx { curFctDef = initCompFunction label : ctx.curFctDef }
  pure $ Right ()


endFunctionComp :: CompileResult
endFunctionComp = do
  ctx <- get
  case ctx.curFctDef of
    [] -> pure . Left $ CompError [(0, "Closing function comp context on an empty list.")]
    hFct : tFcts ->
      let
        newDef = FunctionDef { name = T.decodeUtf8 hFct.name,  args = [], heapDef = 0, body = ByteCode . V.fromList $ concatMap toInstr hFct.bytecode }
        newFctID = fromIntegral $ Mp.size ctx.functions
      in do
      put ctx { curFctDef = tFcts, functions = Mp.insert hFct.name (newDef, newFctID) ctx.functions }
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


compileStatements :: MainText -> [Statement] -> Either CompError CompContext
compileStatements funcName stmts =
  -- TODO: find out how to detect errors and pass on to caller.
  let
    newCtx = initCompContext funcName
    (rezA, finalState) = runState (mapM compileStmt stmts) newCtx
  in
  case concatErrors rezA of
    Nothing -> Right finalState
    Just errs -> Left errs


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


compileStmt (IfST condExpr thenStmt elseStmt) = do
  compileExpression condExpr
  emitOp CMP_BOOL_IMM
  emitOp $ JUMP_TRUE 2
  eiElseLabel <- newLabel
  case eiElseLabel of
    Right elseLabel -> do
      compileStmt thenStmt
      eiEndLabel <- newLabel
      case eiEndLabel of
        Right endLabel -> do
          resolveLabel elseLabel
          compileStmt elseStmt
          resolveLabel endLabel
        _ -> pure $ Left $ CompError [(0, "Error creating end label for if statement.")]
    _ -> pure $ Left $ CompError [(0, "Error creating else label for if statement.")]
    

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
  eiIterLabel <- newLabel
  eiElseLabel <- newLabel
  eiEndLabel <- newLabel
  case (eiIterLabel, eiElseLabel, eiEndLabel) of
    (Right iterLabel, Right elseLabel, Right endLabel) -> do
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
    (a,b,c) -> pure . Left . fromJust $ concatErrors [ a, b, c ]


compileStmt (WithST expr thenStmt elseStmt) = do
  -- TODO: extract type expected from the expr and use it to specialise the context variable type.
  withCtxtID <- registerWithContext (SimpleTypeVT $ StructST "Anything")
  eiElseLabel <- newLabel
  eiEndLabel <- newLabel
  case (eiElseLabel, eiEndLabel) of
    (Right elseLabel, Right endLabel) -> do
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
    (a, b) -> pure . Left . fromJust $ concatErrors [a, b]


compileStmt (ReturnST expr) = do
  compileExpression expr
  -- TODO: put the right number of values to return.
  emitOp $ RETURN 0


compileStmt ContinueST = do
  mbIterLabels <- getIterationLabel
  case mbIterLabels of
    Just (iterLabel, endLabel) -> emitOp $ JUMP_ABS iterLabel
    Nothing -> pure . Left $ CompError [(0, "No active loop to continue.")]


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
  startFunctionComp label
  compileStmt body
  -- TODO: check if the define block returns a value.
  -- TODO: put the right number of values to return.
  emitOp $ RETURN 0
  endFunctionComp
  mdFctID <- getFunctionID label
  case mdFctID of
    Just fctID ->
      emitOp $ REDUCE fctID 0
    Nothing -> pure . Left $ CompError [(0, "@[DefineST] function not found: " <> show label)]


compileStmt (BlockST label contextExpr stmt) = do
  -- TODO: figure out if this makes sense.
  startFunctionComp label
  blockID <- registerBlock label
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  compileExpression contextExpr
  emitOp $ SET_VAR localCtx
  emitOp $ REDUCE blockID 1
  setFunctionContext blockID
  compileStmt stmt
  -- TODO: put the right number of values to return.
  emitOp $ RETURN 0
  endFunctionComp

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
  case concatErrors rezA of
    Nothing -> pure $ Right ()
    Just err -> pure $ Left err


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


compileExpression (ExprVariable var@(Variable kind label)) = do
  -- TODO: extract type expected from the variable.
  case kind of
    LocalK -> do
      varID <- registerVariable var (SimpleTypeVT IntST)
      emitOp $ GET_VAR varID
    MethodK -> do
      localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
      emitOp $ GET_VAR localCtx
      lID <- addStringConstant label
      emitOp $ PUSH_CONST lID
      emitOp GET_FIELD
    LocalMethodK -> do
      -- TODO: understand the difference between MethodK and LocalMethodK.
      localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
      emitOp $ GET_VAR localCtx
      lID <- addStringConstant label
      emitOp $ PUSH_CONST lID
      emitOp GET_FIELD



compileExpression ExprCurrentContext = do
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  emitOp $ GET_VAR localCtx


compileExpression ExprParentContext = do
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  emitOp $ GET_VAR localCtx
  parentLabelID <- addStringConstant "$parentCtx"
  emitOp $ PUSH_CONST parentLabelID
  emitOp GET_FIELD 


compileExpression (ExprMethodAccess fields values) = do
  localCtx <- registerVariable (Variable LocalK "$localCtx") (SimpleTypeVT $ StructST "ExecContext")
  emitOp $ GET_VAR localCtx
  -- TODO: implement properly.
  mapM_ (\(Variable varKind varName) -> do
      lID <- addStringConstant varName
      emitOp $ PUSH_CONST lID
      emitOp GET_FIELD
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

