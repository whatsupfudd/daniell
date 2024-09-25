module RunTime.Compiler.Common where

import Control.Monad.State (State, get, put, modify)
import Control.Monad (foldM)

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Map as Mp
import Data.Text (pack)
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T

import qualified Crypto.Hash.MD5 as Cr

import Conclusion (GenError (..))
import RunTime.Interpreter.Context (ConstantValue (..), FunctionDef (..), FunctionCode (..), FirstOrderType (..), SecondOrderType (..), StructEntries (..))
import RunTime.Interpreter.OpCodes (OpCode (..), opParCount, toInstr)
import Template.Golang.Defines (impModules, impRevModules, impFunctions)
import RunTime.Compiler.Types


initCompContext :: (Show subCtxt) => MainText -> subCtxt -> CompContext subCtxt
initCompContext funcLabel subCtxt = CompContext {
  constants = Mp.empty
  , functions = Mp.empty
  , hasFailed = Nothing
  , unitCounter = 0
  , spitFctID = 0
  , curFctDef = initCompFunction funcLabel :| []
  , subContext = subCtxt
  , functionSlots = Mp.empty
  , moduleMap = impModules
  , revModuleMap = impRevModules
  , importedFcts = impFunctions
}


initCompFunction :: MainText -> CompFunction
initCompFunction aLabel = CompFunction {
      name = aLabel
    , args = []
    , heapDef = initHeapDef
    , varAssignments = Mp.empty
    , opcodes = V.empty
    , returnType = SimpleVT IntST
    , labels = Mp.empty
    , references = Mp.empty
    , iterLabels = []
    , symbols = Mp.empty
  }

initHeapDef :: Mp.Map MainText (Int32, SecondOrderType)
initHeapDef =
  Mp.singleton "$local" (0, StructSO $ NamedSE (
        ( "$parentCtx", FirstOrderSO StringTO )
        :| []
      )
    )

concatErrors :: [Either CompError a] -> Maybe CompError
concatErrors = foldl (\accum eiErr -> case eiErr of
    Left err@(CompError nList) -> case accum of
        Nothing -> Just err
        Just (CompError accumList) -> Just $ CompError (accumList <> nList)
    _ -> accum
  ) Nothing


emitOp :: (Show sc) => OpCode -> GenCompileResult sc
emitOp instr = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef 
    updFct = curFct { opcodes = curFct.opcodes <> V.fromList [instr] }
  put ctx { curFctDef = updFct :| tailFcts }
  pure $ Right ()


newLabel :: (Show sc) => State (CompContext sc) Int32
newLabel = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef 
    labelID = fromIntegral $ Mp.size curFct.labels
    newFctDef = curFct { labels = Mp.insert labelID Nothing curFct.labels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure labelID


setLabelPos :: (Show sc) => Int32 -> GenCompileResult sc
setLabelPos labelID = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef 
  case Mp.lookup labelID curFct.labels of
    Nothing ->
      let
        errMsg = "@[setLabelPos] internal error: label " <> show labelID <> " not found."
      in do
      put ctx { hasFailed = Just . SimpleMsg . pack $ errMsg }
      pure . Left $ CompError [(0, errMsg)]
    Just aPos ->
      let
        opPos = fromIntegral $ V.length curFct.opcodes
        newFctDef = curFct { labels = Mp.insert labelID (Just opPos) curFct.labels }
      in do
      put ctx { curFctDef = newFctDef :| tailFcts }
      pure $ Right ()


addStringConstant :: (Show sc) => MainText -> State (CompContext sc) Int32
addStringConstant newConst =
  addTypedConstant (StringCte newConst) $ Cr.hash newConst

addVerbatimConstant :: (Show sc) => MainText -> State (CompContext sc) Int32
addVerbatimConstant newConst =
  addTypedConstant (VerbatimCte False newConst) $ Cr.hash newConst


addTypedConstant :: (Show sc) => ConstantValue -> MainText -> State (CompContext sc) Int32
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


-- TODO: refactor:
pushIterLabels :: (Show sc) => (Int32, Int32) -> GenCompileResult sc
pushIterLabels iterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    newFctDef = curFct { iterLabels = iterLabels : curFct.iterLabels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure $ Right ()


popIterLabels :: (Show sc) => GenCompileResult sc
popIterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    newFctDef = curFct { iterLabels = tail curFct.iterLabels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure $ Right ()


getIterationLabel :: (Show sc) => State (CompContext sc) (Maybe (Int32, Int32))
getIterationLabel = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
  case curFct.iterLabels of
    [] -> pure Nothing
    h : _ -> pure $ Just h

-- TODO: proper implementation.
referenceIdent :: (Show sc) => MainText -> RefType -> State (CompContext sc) (Maybe Int32)
referenceIdent refName refKind = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
  case Mp.lookup refName curFct.references of
    Just (kind, refID) -> pure $ Just refID
    Nothing -> do
      let
        refID = fromIntegral $ Mp.size curFct.references
        updFct = curFct { references = Mp.insert refName (refKind, refID) curFct.references }
      put ctx { curFctDef = updFct :| tailFcts }
      pure $ Just refID


getFunctionSlot :: (Show sc) => MainText -> State (CompContext sc) Int32
getFunctionSlot funcName = do
  ctx <- get
  case Mp.lookup funcName ctx.functionSlots of
    Just (_, funcID) -> pure funcID
    Nothing -> do
      let
        funcID = fromIntegral $ 1 + Mp.size ctx.functionSlots
      put ctx { functionSlots = Mp.insert funcName (UnresolvedFR, funcID) ctx.functionSlots }
      pure funcID


getImportedFunction :: (Show sc) => MainText -> [ CompType ] -> State (CompContext sc) (Maybe [(FunctionDefComp, Int32)])
getImportedFunction funcName argTypes= do
  -- TODO: implement the argTypes matching.
  ctx <- get
  pure $ Mp.lookup funcName ctx.importedFcts


pushFunctionComp :: (Show sc) => MainText -> GenCompileResult sc
pushFunctionComp label = do
  ctx <- get
  -- TODO: check that the label exists in the functions map.
  put ctx { curFctDef = initCompFunction label <| ctx.curFctDef }
  pure $ Right ()


popFunctionComp :: (Show sc) => GenCompileResult sc
popFunctionComp = do
  ctx <- get
  case ctx.curFctDef of
    curFct :| (hTail : tTail) ->
      let
        curFct :| tailFcts = ctx.curFctDef
        newFctID = fromIntegral $ Mp.size ctx.functions
      in do
      put ctx { curFctDef = hTail :| tTail, functions = Mp.insert curFct.name (curFct, newFctID) ctx.functions }
      pure $ Right ()
    _ -> pure $ Left $ CompError [(0, "Closing function comp context on an empty list.")]


-- TODO: implement.
setFunctionContext :: (Show sc) => Int32 -> GenCompileResult sc
setFunctionContext funcID = pure $ Right ()
