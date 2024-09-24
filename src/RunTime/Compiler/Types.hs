module RunTime.Compiler.Types where

import Control.Monad.State (State)

import qualified Data.ByteString as Bs
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Vector as V


import Conclusion (GenError (..))
import RunTime.Interpreter.Context (ConstantValue (..), FunctionDef (..), FunctionCode (..), HeapEntry (..), SecondOrderType)
import RunTime.Interpreter.OpCodes (OpCode (..), opParCount, toInstr)

type MainText = Bs.ByteString

data (Show subCtxt) => CompContext subCtxt = CompContext {
    constants :: Mp.Map MainText (ConstantValue, Int32)
    , functions :: Mp.Map MainText (CompFunction, Int32)
    , hasFailed :: Maybe GenError
    , unitCounter :: Int32
    , spitFctID :: Int32
    , curFctDef :: NonEmpty CompFunction
    , functionSlots :: Mp.Map MainText (FunctionRef, Int32)
    , subContext :: subCtxt
  }


data FunctionRef =
  ExternalFR MainText SecondOrderType
  | InternalFR Int32
  | UnresolvedFR
  deriving Show


instance (Show subCtxt) => Show (CompContext subCtxt) where
  show c = "CompContext {\n    constants = ["
      <> concatMap (\cte -> "\n      , " <> show cte) (L.sortOn snd $ Mp.elems c.constants)
      <> "\n], functionSlots: " <> show c.functionSlots
      <> "\n  , functions = [" <> concatMap (\c -> "\n      , " <> show c) c.functions
      <> "\n]  , hasFailed = " <> show c.hasFailed
      <> "\n  , subContext = " <> show c.subContext
      <> ", curFctDef = " <> show c.curFctDef <> "\n}"


data CompFunction = CompFunction {
    name :: MainText
    , args :: [ (MainText, VarType) ]
    , heapDef :: Mp.Map MainText (Int32, SecondOrderType)
    , varAssignments :: Mp.Map MainText Int32
    , opcodes :: V.Vector OpCode
    , returnType :: VarType
    , labels :: Mp.Map Int32 (Maybe Int32)
    , references :: Mp.Map MainText (RefType, Int32)
    , iterLabels :: [ (Int32, Int32) ]
    , symbols :: Mp.Map MainText Int32
  }

instance Show CompFunction where
  show f = 
    let
      revLabels = Mp.foldrWithKey (\k mbV acc -> case mbV of Nothing -> acc; Just v -> Mp.insert v k acc) (Mp.empty :: Mp.Map Int32 Int32) f.labels
    in
    "CompFunction {\n    name = " <> show f.name
      <> "\n  , args = " <> show f.args
      <> "\n  , heapDef = " <> show f.heapDef
      <> "\n  , varAssignments = " <> show f.varAssignments
      <> "\n  , opcodes = [" 
        <> fst (V.foldl (\(acc, addr) op -> (acc <> "\n      , " <> showOpcode revLabels addr op, succ addr)) ("", 0) f.opcodes)
      <> "\n]"
      <> "\n  , returnType = " <> show f.returnType
      <> "\n  , labels = " <> show f.labels
      <> "\n  , references = " <> show f.references
      <> "\n  , iterLabels = " <> show f.iterLabels
      <> "\n  , symbols = " <> show f.symbols <> "\n}"

showOpcode :: Mp.Map Int32 Int32 -> Int32 -> OpCode -> String
showOpcode revLabels addr op =
  let
    addrTxt = case Mp.lookup addr revLabels of
      Just j -> show j <> "/"
      Nothing -> ""
  in
  addrTxt <> show addr <> ": " <> show op


data RefType =
  VarRT
  | FunctionRT
  | MethodRT
  deriving Show


newtype CompError = CompError [(Int32, String)]
  deriving Show


data VarType =
  SimpleVT SimpleType
  | MonadicVT MainText [ VarType ]    -- Eg [ Int ], Maybe String, etc.
  | StructVT MainText
  | UnknownVT
  deriving Show


data SimpleType =
  IntST
  | FloatST
  | BoolST
  | StringST
  deriving Show


type GenCompileResult subCtxt = State (CompContext subCtxt) (Either CompError ())

