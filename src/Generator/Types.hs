{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Generator.Types where

import Control.Monad (foldM)

import Data.Text (Text, pack)
import Data.List.NonEmpty (NonEmpty)

import Conclusion (GenError (..))
import Options.Runtime (RunOptions (..))
import FileSystem.Types (FileItem, FileKind)
import Scaffold.Types (ScaffoldBundle)
import Data.HashMap.Internal.Array (run)


data ContextValue =
  TemplItem ScaffoldBundle
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
  runPlan rtOpts engine context =
    foldM (runItem engine) (Right context)
    where
    runItem :: (ExecSystem e c w) => e -> Either GenError c -> w -> IO (Either GenError c)
    runItem _ (Left err) item = pure $ Left err
    runItem engine (Right ctxt) item = runWorkItem rtOpts engine ctxt item

