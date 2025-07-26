module Scaffold.Types where

import Data.Text (Text)

import Cannelle.Templog.Types (ParameterMap, Function)

import FileSystem.Types (PathFiles)

{- Scaffhold Template
 Defines how to assemble a project (currently, Haskell app) as a set of files and directories.
 The files are typically Haskell code that can be extended with '{{' and '}}' blocks of templating logic
 that is evaluated to produce different results based on the options & parameters provided upon launching the
 project creation.
-}


data ScaffoldBundle = ScaffoldBundle {
  path :: FilePath
  , hasPrefix :: Maybe FilePath
  , description :: Maybe Text
  , structure :: PathFiles
  , parameters :: ParameterMap
  , logic :: Function
  }
  deriving Show
