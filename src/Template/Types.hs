{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Template.Types where

import Data.Text (Text)
import qualified Data.Map as Mp
import FileSystem.Types (PathFiles)

{- Project Template -}

data ProjectTempl = ProjectTempl {
  path :: FilePath
  , hasPrefix :: Maybe FilePath
  , description :: Maybe Text
  , structure :: PathFiles
  , parameters :: ParameterMap
  , logic :: Function
  }
  deriving Show

{- File Template -}

data FileTempl = FileTempl {
  path :: FilePath
  , description :: Maybe Text
  , parameters :: ParameterMap
  , logic :: Function
  }
  deriving Show

{- Function -}

type ParameterMap = Mp.Map Text Parameter

data Parameter =
  NumberP Int
  | StringP Text
  | BoolP Bool
  | ListP [ Parameter ]
  | TypeP Text
  deriving Show

{- Logic: move to the Generator section? -}
data Function =
  Concat Text
  | Exec Code
  | Sequence [ Function ]
  | Noop
  deriving Show

data NameBinding = NameBinding {
    name :: Text
    , vType :: TypeDef
}
  deriving Show

data TypeDef =
  NumberT
  | StringT
  | BoolT
  | ListT TypeDef
  | TupleT [ TypeDef ]
  | RecordT [ (Text, TypeDef) ]
  | FunctionT [ TypeDef ] TypeDef
  deriving Show

data Code =
  -- bounded value:
  Reference Text
  -- Function execution: bounded function, values to be passed.
  | Application Text [ Code ]
  -- Function definition:
  | Lambda [ NameBinding ] Code
  -- Functional name binding and application in logic block (functional sequencing):
  | Let [ (Text, Code) ] Code
  -- Conditional execution: test, block-if-sucessful, block-if-failed.
  | Case Code [ (Code, Code) ]
  -- Assignment of a name to a value (or re-assignment for mutable logic):
  | Bind NameBinding Code
  deriving Show


{-- TODO:
 * Add:
  - origin of content
  - format of output (html, amp, xml)
  - parent pointer
  - children templates
  - ? compiled logic ?
-}

{- Old stuff -}
data Template = Template {
    origin :: FilePath
  }

data DirTreeNode = DirTreeNode {
    name :: Text
  , children :: [ DirTreeNode ]
  }
  deriving Show

