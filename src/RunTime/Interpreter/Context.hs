module RunTime.Interpreter.Context

where

import qualified Data.ByteString as Bs
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Map as Mp

import RunTime.Interpreter.OpCodes (OpCode (..), dissassemble)
import RunTime.Interpreter.Memory (IntM)

type MainText = Bs.ByteString


data VmContext = VmContext {
    status :: StatusVM
    , frameStack :: NonEmpty Frame
    , outStream :: BS.ByteString
    , modules :: V.Vector VMModule
    -- TODO: decide if this is useful given each module has a constants vector.
    , constants :: V.Vector ConstantValue
  }
  deriving Show


data StatusVM =
  Running
  | Init
  | Halted
  | Crashed String
  deriving Show


type StackValue = (StackEntry, IntM)
type Stack = [ StackValue ]
type Heap = V.Vector HeapEntry


data StackEntry =
  BoolSV
  | CharSV
  | IntSV
  | FloatSV
  | HighLongSV
  | LowLongSV
  | HighDoubleSV
  | LowDoubleSV
  | StringSV
  | ConstantRefSV
  | HeapRefSV
  deriving Show

data FirstOrderType =
  BoolTO
  | CharTO
  | IntTO
  | FloatTO
  | HighLongTO
  | LowLongTO
  | HighDoubleTO
  | LowDoubleTO
  | StringTO
  deriving Show

data SecondOrderType =
  FirstOrderSO FirstOrderType
  -- For containers (Array, Tuple, etc).
  | MonadicSO SecondOrderType
  | StructSO StructEntries
  -- For functions: the first element in the list is the return, and then backward to the first argument.
  | LambdaSO (NonEmpty SecondOrderType)
  -- For dynamic values which type is discovered on the fly.
  | DynamicSO
  -- For a generic type, known at runtime (but once known, it's fixed).
  | VarTypeSO
  deriving Show


data StructEntries =
  AnonymousSE (NonEmpty SecondOrderType)
  | NamedSE (NonEmpty (MainText, SecondOrderType))
  deriving Show


data HeapEntry =
  BoolHE Bool
  | CharHE Char
  | IntHE Int32
  | FloatHE Float
  | LongHE Int64
  | DoubleHD Double
  | StringHE BS.ByteString
  | ArrayHE (V.Vector HeapEntry)
  | TupleHE (V.Vector HeapEntry)
  -- StructHE: first in is the ID of the struct in the ConstantPool, the list is its fields values.
  | StructHE Int32 [HeapEntry]
  deriving Show


data Frame = Frame {
    stack :: Stack
    , heap :: Heap
    , function :: FunctionDef
    , pc :: Int
    , flags :: CompareFlags
    , returnValue :: (Maybe Int, Maybe StackValue)
  }
  deriving Show

data CompareFlags =
  NoFlag
  | EqFlag
  | NeFlag
  | LtFlag
  | LeFlag
  | GeFlag
  | GtFlag
  | TrueFlag
  | FalseFlag
  deriving (Eq, Show)


-- ModuleDef: implements a "a.b.c" module naming, with an ID for the label, and a possible ref to its parent ID.
type ModuleDef = Mp.Map Int32 (MainText, Maybe Int32)
-- DerefModuleDef: for finding a module in ModuleDef from a leaf name; it tracks all labels inserted in ModuleDef and
--  their parent ID to differentiate between same labels in different sequences.
type DerefModuleDef = Mp.Map MainText [(Int32, Maybe Int32)]

data ModuleRepo = ModuleRepo {
  naming :: ModuleDef
  , dereference :: DerefModuleDef
  , modules :: V.Vector VMModule
}


data ModuledDefinition = ModuledDefinition {
    modName :: Text
    , modBody :: Mp.Map Text FunctionDef
  }
  deriving Show


-- representation of a runtime module:
data VMModule = VMModule {
    functions :: V.Vector FunctionDef
    , constants :: V.Vector ConstantValue
    , externModules :: Mp.Map Text ModuledDefinition
  }
  deriving Show


data FunctionDef = FunctionDef {
    moduleID :: Int32
    , fname :: Text
    , args :: Maybe (NonEmpty ArgumentDef)
    , returnType :: SecondOrderType
    , body :: FunctionCode
  }

instance Show FunctionDef where
  show (FunctionDef mid n a r b) = "FunctionDef " <> show n <> "\n  , code: "
      <> case b of
           NativeCode ref -> "native: " <> ref <> "."
           ByteCode code -> dissassemble code


data ArgumentDef = ArgumentDef {
    name :: Text
    , atype :: SecondOrderType
  }
  deriving Show

data FunctionCode =
  -- NativeCode: a function not implemented in Daniell VM's bytecode; the string is for finding that function
  -- in some library dereferencing system.
  NativeCode String
  | ByteCode (V.Vector Int32)
  deriving Show


-- VerbatimCte: can be compressed (true), while StringCte is not.
data ConstantValue =
  VerbatimCte Bool BS.ByteString
  | StringCte BS.ByteString
  | IntCte Int
  | FloatCte Float
  | DoubleCte Double
  | ArrayCte [ ConstantValue ]
  | TupleCte [ ConstantValue ]
  | ClassCte MainText
  | NameAndType MainText MainText
  | TypeSignature MainText
  | FieldRef MainText MainText
  | MethodRef MainText MainText
  | FunctionRef MainText
  deriving Show

