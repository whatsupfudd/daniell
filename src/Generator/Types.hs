{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Generator.Types where

import Data.Text (Text, pack)

import Conclusion (GenError (..))
import Options.Runtime (RunOptions (..))
import FileSystem.Types (FileItem, FileKind)
import Template.Types (ScaffoldTempl)
import Data.HashMap.Internal.Array (run)
import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty)


data ContextValue =
  TemplItem ScaffoldTempl
  | FileItem FileItem
  | PathItem FilePath
  deriving Show


-- WorkPlan data type
data WorkPlan e c w = WorkPlan { 
      engine :: e
    , context :: c
    , items :: NonEmpty w
  }
  deriving Show


class ExecSystem e c w where
  runWorkItem :: RunOptions -> e -> c -> w -> IO (Either GenError c)

  runPlan :: RunOptions -> e -> c -> NonEmpty w -> IO (Either GenError c)
  runPlan rtOpts engine context = foldM (runItem engine) (Right context)
    where
    runItem :: (ExecSystem e c w) => e -> Either GenError c -> w -> IO (Either GenError c)
    runItem _ (Left err) item = pure $ Left err
    runItem engine (Right ctxt) item = runWorkItem rtOpts engine ctxt item


type ScfWorkPlan = WorkPlan ScfEngine ScfContext ScfWorkItem

data ScfEngine = ScfEngine
  deriving Show

data ScfContext = ScfContext {
    scaffoldTempl :: ScaffoldTempl
  , destDir :: FilePath
  }
  deriving Show

data ScfWorkItem =
  NewDirIfNotExist FilePath
  -- parse source, exec and save result to destination, based on: source type, source location, destination location
  | DupFromSource FileItem FilePath FilePath
  -- straight copy from source to destination:
  | CloneSource FilePath FilePath
  -- run a 'logic' template, no implied result.AnonymousRoutes
  | RunTemplate FilePath
  | RunTemplateToDest FileKind FilePath FileItem FilePath
  | ConfigWith FileItem
  deriving Show
