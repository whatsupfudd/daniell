module RunTime.Interpreter.Context

where

import Data.Array (Array)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Map as Mp

import RunTime.Interpreter.OpCodes (OpCode (..))
import RunTime.Interpreter.Memory (IntM)


data Variable = Variable
  deriving Show


data CodeBlock = CodeBlock {
    instructions :: Array Int32 OpCode
    , labels :: Mp.Map String Int32
  }


newtype VerbatimBlock = VerbatimBlock {
    blocks :: Array Int32 (Int32, Int32)
  }

data StatusVM =
  Running
  | Init
  | Halted
  deriving Show


data VmContext = VmContext {
    status :: StatusVM
    , frame :: Frame
    , frameStack :: [ Frame ]
    , outStream :: BS.ByteString
    , modules :: V.Vector VMModule
    -- TODO: decide if this is useful given each module has a constants vector.
    , constants :: V.Vector ConstantValue
  }
  deriving Show

type StackValue = (TypeSV, IntM)
type HeapValue = ConstantValue
type Stack = [ StackValue ]
type Heap = V.Vector HeapValue

data TypeSV =
  BoolSV
  | CharSV
  | IntSV
  | FloatSV
  | HighLongSV
  | LowLongSV
  | HighDoubleSV
  | LowDoubleSV
  | StringSV
  | ArraySV
  | TupleSV
  | ConstantRefSV
  | HeapRefSV
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


-- representation of a runtime module:
data VMModule = VMModule {
    functions :: V.Vector FunctionDef
    , constants :: V.Vector ConstantValue
    , externModules :: Mp.Map Text ModuledDefinition
  }
  deriving Show


data FunctionDef = FunctionDef {
    name :: Text
    , args :: [ Text ]
    , heapDef :: Int32
    , body :: FunctionCode
  }
  deriving Show


data FunctionCode =
  NativeCode
  | ByteCode (V.Vector Int32)
  deriving Show


data ModuledDefinition = ModuledDefinition {
    modName :: Text
    , modBody :: Mp.Map Text FunctionDef
  }
  deriving Show

data ConstantValue =
  StringCte BS.ByteString
  | VerbatimCte BS.ByteString
  | IntCte Int
  | FloatCte Float
  | DoubleCte Double
  | ArrayCte [ ConstantValue ]
  | TupleCte [ ConstantValue ]
  deriving Show

