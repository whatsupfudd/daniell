{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Template.Types where

import Data.Text (Text)
import qualified Data.Map as Mp
import FileSystem.Types (PathFiles)

import qualified RunTime.Interpreter.Context as Vm


{- Scaffhold Template
 Defines how to assemble a project (currently, Haskell app) as a set of files and directories.
 The files are typically Haskell code that can be extended with '{{' and '}}' blocks of templating logic
 that is evaluated to produce different results based on the options & parameters provided upon launching the
 project creation.
-}

data ScaffholdTempl = ScaffholdTempl {
  path :: FilePath
  , hasPrefix :: Maybe FilePath
  , description :: Maybe Text
  , structure :: PathFiles
  , parameters :: ParameterMap
  , logic :: Function
  }
  deriving Show

{- File Template
  Defines a sequence of verbatim content and logic blocks. The verbatim content is simply concatenated toward the
  output, while the logic blocks are evaluated and control the generation of additional output that is added in
  the stream of output.
  The following makes up the 'compiled module' information:
   - *context* of the template is the global values that are part of the template and available to the logic blocks for
  controlling the processing.
   - *logic* is the sequence of functions defined in the template (the .text segment of an object file).
   - *constants* are the literal values extracted from the functions and accumulated in a unique & global list (the equivalent
  of a .data segment in an object file).
-}

data FileTempl = FileTempl {
  path :: FilePath
  , description :: Maybe Text
  , context :: ParameterMap
  , logic :: [ Function ]
  , constants :: [ Parameter ]
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
  | Exec Vm.VMModule
  | Sequence [ Function ]
  | Noop
  | CloneVerbatim FilePath
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

