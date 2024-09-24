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

import RunTime.Compiler.Types (CompError (..), CompContext (..), GenCompileResult (..), MainText, VarType (..), SimpleType (..))
import qualified RunTime.Compiler.Types as C
import qualified RunTime.Compiler.Common as C
import RunTime.Interpreter.OpCodes
import Text.Cannelle.Hugo.AST

data HugoCompileCtxt = HugoCompileCtxt {
    internalTemplates :: Mp.Map MainText Int32
    , externalTemplates :: Mp.Map MainText Int32
    , blocks :: Mp.Map MainText Int32
  }
  deriving Show

type FullCompContext = CompContext HugoCompileCtxt
type CompileResult = GenCompileResult HugoCompileCtxt


--- *** Utility functions *** ---
initHugoCompileCtxt :: HugoCompileCtxt
initHugoCompileCtxt = HugoCompileCtxt {
  internalTemplates = Mp.empty
  , externalTemplates = Mp.empty
  , blocks = Mp.empty
}

-- TODO: implement.
registerVariable :: Variable -> VarType -> State FullCompContext Int32
registerVariable (Variable varKind label) varType = pure 1
-- VarKind = LocalK ($aVar) | MethodK (.aMethod) | LocalMethodK ($.aMethod)

-- TODO: implement.
dereferVariable :: Variable -> State FullCompContext (Maybe Int32)
dereferVariable label = pure Nothing

-- TODO: implement.
registerBlock :: MainText -> State FullCompContext Int32
registerBlock label = pure 0

-- TODO: implement.
getInternalTemplate :: MainText -> State FullCompContext Int32
getInternalTemplate label = pure 0

-- TODO: implement.
getExternalTemplate :: MainText -> State FullCompContext Int32
getExternalTemplate label = pure 0


-- *** AST to OpCodes logic *** ---
compileStatements :: MainText -> [Statement] -> Either CompError FullCompContext
compileStatements funcName stmts =
  -- TODO: find out how to detect errors and pass on to caller.
  let
    newCtx = C.initCompContext funcName initHugoCompileCtxt
    (rezA, finalState) = runState (mapM compileStmt stmts) newCtx
  in
  case C.concatErrors rezA of
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
        Nothing -> Right <$> registerVariable var UnknownVT
  case eiVarID of
    Left err -> pure $ Left err
    Right varID -> do
      compileExpression expr
      C.emitOp $ SET_VAR varID


compileStmt (VerbatimST text) = do
  cteID <- C.addVerbatimConstant text
  C.emitOp $ PUSH_CONST cteID
  ctx <- get
  C.emitOp $ REDUCE ctx.spitFctID 1


compileStmt (IfST condExpr thenStmt elseStmt) = do
  elseLabel <- C.newLabel
  endLabel <- C.newLabel
  compileExpression condExpr
  C.emitOp CMP_BOOL_IMM
  C.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  compileStmt thenStmt
  C.emitOp $ JUMP_ABS (LabelRef endLabel)
  C.setLabelPos elseLabel
  compileStmt elseStmt
  C.setLabelPos endLabel
    

compileStmt (RangeST mbVars expr thenStmt elseStmt) = do
  mbValIDs <- case mbVars of
    Just (RangeVars valVar mbIdxVar) -> do
      -- TODO: extract type expected from the variable.
      valID <- registerVariable valVar UnknownVT
      mbIdxID <- case mbIdxVar of
        Nothing -> pure Nothing
        Just aVar -> Just <$> registerVariable aVar (SimpleVT IntST)
      pure $ Just (valID, mbIdxID)
    Nothing -> pure Nothing
  iterLabel <- C.newLabel
  elseLabel <- C.newLabel
  endLabel <- C.newLabel
  -- TODO: figure out how to handle the iterator's implicit looping index variable.
  compileIterator iterLabel mbValIDs expr
  C.emitOp CMP_BOOL_IMM
  C.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  C.pushIterLabels (iterLabel, endLabel)
  compileStmt thenStmt
  C.emitOp $ JUMP_ABS (LabelRef iterLabel)
  C.setLabelPos elseLabel
  compileStmt elseStmt
  C.setLabelPos endLabel
  C.popIterLabels


compileStmt (WithST expr thenStmt elseStmt) = do
  -- TODO: extract type expected from the expr and use it to specialise the context variable type.
  withCtxtID <- registerWithContext (StructVT "Anything")
  elseLabel <- C.newLabel
  endLabel <- C.newLabel
  compileExpression expr
  C.emitOp DUP_1
  C.emitOp CMP_BOOL_IMM
  C.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  C.emitOp $ SET_VAR withCtxtID
  compileStmt thenStmt
  C.emitOp $ JUMP_ABS (LabelRef endLabel)
  C.emitOp $ SET_VAR withCtxtID
  C.setLabelPos elseLabel
  compileStmt elseStmt
  C.setLabelPos endLabel
  where
  -- TODO: implement; it returns the index of a new local variable that will hold the localContext within the
  -- with-block. These can work as a stack.
  registerWithContext :: VarType -> State FullCompContext Int32
  registerWithContext varType = pure 1


compileStmt (ReturnST expr) = do
  compileExpression expr
  -- TODO: put the right number of values to return.
  C.emitOp $ RETURN 0


compileStmt ContinueST = do
  mbIterLabels <- C.getIterationLabel
  case mbIterLabels of
    Just (iterLabel, endLabel) -> C.emitOp $ JUMP_ABS (LabelRef iterLabel)
    Nothing -> pure . Left $ CompError [(0, "No active loop to continue.")]


compileStmt BreakST = do
  mbIterLabels <- C.getIterationLabel
  case mbIterLabels of
    Just (iterLabel, endLabel) -> C.emitOp $ JUMP_ABS (LabelRef endLabel)


compileStmt (ExpressionST expr) = do
  ctx <- get
  compileExpression expr
  C.emitOp FORCE_TO_STRING
  C.emitOp $ REDUCE ctx.spitFctID 1


compileStmt (DefineST label body) = do
  C.pushFunctionComp label
  compileStmt body
  -- TODO: check if the define block returns a value.
  -- TODO: put the right number of values to return.
  C.emitOp $ RETURN 0
  C.popFunctionComp


compileStmt (BlockST label contextExpr stmt) = do
  -- TODO: figure out if this makes sense.
  C.pushFunctionComp label
  blockID <- registerBlock label
  localCtx <- registerVariable (Variable LocalK "$blockCtx") (StructVT "ExecContext")
  compileExpression contextExpr
  C.emitOp $ SET_VAR localCtx
  C.emitOp $ REDUCE blockID 1
  C.setFunctionContext blockID
  compileStmt stmt
  -- TODO: put the right number of values to return.
  C.emitOp $ RETURN 0
  C.popFunctionComp

compileStmt (IncludeST label expr) = do
  templateID <- getInternalTemplate label
  compileExpression expr
  C.emitOp $ REDUCE templateID 1


compileStmt (PartialST label expr) = do
  templateID <- getExternalTemplate label
  compileExpression expr
  C.emitOp $ REDUCE templateID 1


compileStmt (ListST stmts) = do
  rezA <- mapM compileStmt stmts
  case C.concatErrors rezA of
    Nothing -> pure $ Right ()
    Just err -> pure $ Left err


compileStmt NoOpST = pure $ Right ()


compileIterator :: Int32 -> Maybe (Int32, Maybe Int32) -> Expression -> CompileResult
compileIterator iterLabel Nothing expr = do
  compileExpression expr
  -- TODO: implement the iterator over the initial expression.
  C.setLabelPos iterLabel

compileIterator iterLabel (Just (idxVarID, Nothing)) expr = do
  compileExpression expr
  C.setLabelPos iterLabel
  -- TODO: implement the iterator over the initial expression.
  C.emitOp IINC_1
  C.emitOp $ SET_VAR idxVarID
  C.emitOp $ SET_VAR idxVarID

compileIterator iterLabel (Just (idxVarID, Just varID)) expr = do
  C.emitOp $ SET_VAR_IM1 idxVarID
  compileExpression expr
  C.setLabelPos iterLabel
  -- TODO: implement the iterator over the initial expression.
  C.emitOp $ SET_VAR varID
  C.emitOp $ GET_VAR idxVarID
  C.emitOp IINC_1
  C.emitOp $ SET_VAR idxVarID


compileExpression :: Expression -> CompileResult
compileExpression (ExprLiteral lit) = case lit of
    LitString s -> do
      cteID <- C.addStringConstant s
      C.emitOp $ PUSH_CONST cteID
    LitNumber n -> C.emitOp $ PUSH_DOUBLE_IMM n
    LitBool b -> C.emitOp $ PUSH_BOOL_IMM b


compileExpression (ExprVariable var@(Variable kind label)) = do
  -- TODO: extract type expected from the variable.
  case kind of
    LocalK -> do
      varID <- registerVariable var (SimpleVT IntST)
      C.emitOp $ GET_VAR varID
    MethodK -> do
      localCtx <- registerVariable (Variable LocalK "$localCtx") (StructVT "ExecContext")
      C.emitOp $ GET_VAR localCtx
      lID <- C.addStringConstant label
      C.emitOp $ PUSH_CONST lID
      C.emitOp GET_FIELD
    LocalMethodK -> do
      -- TODO: understand the difference between MethodK and LocalMethodK.
      localCtx <- registerVariable (Variable LocalK "$localCtx") (StructVT "ExecContext")
      C.emitOp $ GET_VAR localCtx
      lID <- C.addStringConstant label
      C.emitOp $ PUSH_CONST lID
      C.emitOp GET_FIELD


-- TODO: assess that the current context is always at the start of the heap.
compileExpression ExprCurrentContext = do
  C.emitOp $ GET_VAR 0


compileExpression ExprParentContext = do
  contLabel <- C.newLabel
  lID <- C.addStringConstant "$parentCtx"
  C.emitOp $ GET_VAR 0
  C.emitOp $ PUSH_CONST lID
  C.emitOp GET_FIELD
  C.emitOp $ JUMP_FALSE (LabelRef contLabel)
  C.emitOp $ THROW_ERR 1
  C.setLabelPos contLabel


compileExpression (ExprMethodAccess fields values) = do
  C.emitOp $ GET_VAR 0
  -- TODO: implement properly.
  mapM_ (\(Variable varKind varName) -> do
      lID <- C.addStringConstant varName
      C.emitOp $ PUSH_CONST lID
      C.emitOp GET_FIELD
    ) fields
  mapM_ compileExpression values
  C.emitOp $ CALL_METHOD (fromIntegral $ length values)


compileExpression (ExprFunctionCall funcName args) = do
  functionID <- C.getFunctionSlot funcName
  rezA <- mapM compileExpression args
  case splitResults rezA of
    (Just err, _) -> pure $ Left err
    (Nothing, argsIDs) ->
      C.emitOp $ REDUCE functionID (fromIntegral $ length args)


compileExpression (ExprPipeline expr functions) = do
  compileExpression expr
  rezB <- mapM (\(FunctionApplication funcName args) -> do
      functionID <- C.getFunctionSlot funcName
      -- TODO: check for errors in rezA.
      rezA <- mapM compileExpression args
      case splitResults rezA of
        (Just err, _) -> pure $ Left err
        (Nothing, argsIDs) ->
          C.emitOp $ REDUCE functionID (1 + fromIntegral (length args))
    ) functions
  case splitResults rezB of
    (Just err, _) -> pure $ Left err
    (Nothing, _) -> pure $ Right ()


splitResults :: [Either CompError a] -> (Maybe CompError, [a])
splitResults results =
  let
    (lefts, rights) = foldl (\(accE, accA) rez -> case rez of
        Left err -> (Left err : accE, accA)
        Right valA -> (accE, valA : accA)
      ) ([], []) results
  in
  (C.concatErrors lefts, rights)
