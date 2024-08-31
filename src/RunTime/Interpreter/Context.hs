module RunTime.Interpreter.Context

where

import Data.Array (Array)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as Vc
import qualified Data.ByteString as BS
import qualified Data.Map as Mp

import RunTime.Interpreter.OpCodes (OpCode (..))

data Variable = Variable
  deriving Show


data CodeBlock = CodeBlock {
    instructions :: Array Int32 OpCode
    , labels :: Mp.Map String Int32
  }


data VerbatimBlock = VerbatimBlock {
    blocks :: Array Int32 (Int32, Int32)
  }

data StatusVM =
  Running
  | Init
  | Halted
  deriving Show


data VmContext = VmContext {
    globalVars :: [ Variable ]
    , outStream :: BS.ByteString
    , status :: StatusVM
  }
  deriving Show


defaultVM = VmContext [] BS.empty Init

-- representation of a runtime module:
data VMModule = VMModule {
    functions :: [ FunctionDef ]
    , constants :: Vc.Vector ConstantValue
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
  | ByteCode (Vc.Vector Int32)
  deriving Show


data ModuledDefinition = ModuledDefinition {
    modName :: Text
    , modBody :: Mp.Map Text FunctionDef
  }
  deriving Show

data ConstantValue =
  StringCte BS.ByteString
  | IntCte Int
  | FloatCte Float
  | DoubleCte Double
  | ArrayCte [ ConstantValue ]
  | TupleCte [ ConstantValue ]
  deriving Show


