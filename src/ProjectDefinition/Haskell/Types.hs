module ProjectDefinition.Haskell.Types where

import qualified Data.Map as Mp

import qualified FileSystem.Types as Fs


data BuildInfo = BuildInfo {
  stack :: Maybe FilePath
  , cabal :: Maybe FilePath
  , package :: Maybe FilePath
  }
  deriving (Show)


data HaskellComponents = HaskellComponents {
  buildInfo :: Mp.Map FilePath BuildInfo
  , modules :: [ (FilePath, Fs.FileItem) ]
  , otherLogic :: [ (FilePath, Fs.FileItem) ]
  }
  deriving (Show)

