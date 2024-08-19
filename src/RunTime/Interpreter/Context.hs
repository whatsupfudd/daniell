module RunTime.Interpreter.Context

where

import Data.Array (Array)
import Data.Int (Int32, Int64)
import qualified Data.ByteString as BS
import qualified Data.Map as Mp

import RunTime.Interpreter.OpCodes (OpCode (..))

data Variable = Variable


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


data VmContext = VmContext {
    globalVars :: [ Variable ]
    , outStream :: BS.ByteString
    , status :: StatusVM
  }


defaultVM = VmContext [] BS.empty Init