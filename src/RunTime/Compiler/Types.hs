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
    -- functions: fully parsed functions.
    , functions :: Mp.Map MainText (CompFunction, Int32)
    , hasFailed :: Maybe GenError
    , unitCounter :: Int32
    , spitFctID :: Int32
    , curFctDef :: NonEmpty CompFunction
    , functionSlots :: Mp.Map MainText (FunctionRef, Int32)
    , subContext :: subCtxt
    , moduleMap :: Mp.Map Int32 (MainText, Maybe Int32)
    , revModuleMap :: Mp.Map MainText [(Int32, Maybe Int32)]
    , importedFcts :: Mp.Map MainText [ (FunctionDefComp, Int32) ]
  }


-- TODO: move to the RunTime module:
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
      <> ", curFctDef = " <> show c.curFctDef
      <> "\n  , importedFcts = " <> show c.importedFcts
      <> "\n}"


-- Representation of a function being compiled:
data CompFunction = CompFunction {
    name :: MainText
    , args :: [ (MainText, CompType) ]
    , heapDef :: Mp.Map MainText (Int32, SecondOrderType)
    , varAssignments :: Mp.Map MainText Int32
    , opcodes :: V.Vector OpCode
    , returnType :: CompType
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
      Just j -> show j <> " > "
      Nothing -> ""
  in
  addrTxt <> show addr <> ":\t" <> show op


data RefType =
  VarRT
  | FunctionRT
  | MethodRT
  deriving Show


newtype CompError = CompError [(Int32, String)]
  deriving Show


-- Compilation-time types:
data CompType =
  SimpleVT SimpleType
  | MonadicVT [ CompType ]    -- Containers: array, map, slice.
  | StructVT [ StructField ]
  | LambdaVT CompType [ (MainText, CompType) ]
  -- Undecided yet during compilation:
  | UnknownVT
  -- Dynamic type, anything fits and the type is decided at runtime:
  | DynamicVT
  -- Special type of variable, all other arguments are combined into an array of values:
  deriving Show


data SimpleType =
  IntST
  | FloatST
  -- Generalize the Int & Float types:
  | NumberST
  | BoolST
  | StringST
  deriving Show


data StructField =
  AnonymousSF CompType
  | NamedSF MainText CompType
  deriving Show


-- **** Imported module/function definitions **** --
data FunctionDefComp = FunDef {
    iModule :: Int32
    , fname :: MainText
    , def :: SignaturePart
  }
  deriving Show
data SignaturePart =
  MonoDef Signature
  | PolyDef [ Signature ]
  deriving Show

data Signature = SgnS {
    args :: Maybe (NonEmpty (MainText, CompType))
    , optArgs :: [ (MainText, CompType) ]
    , globber :: Maybe (MainText, CompType)
    , retType :: CompType
  }
  deriving Show


-- Overall state container for the compiation process:
type GenCompileResult subCtxt = State (CompContext subCtxt) (Either CompError ())

