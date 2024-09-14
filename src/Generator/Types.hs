{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Generator.Types where

import Data.Text (Text, pack)

import FileSystem.Types (FileItem, FileKind)
import Conclusion (GenError (..))

class Show engineT => Engine engineT where
  run :: engineT -> IO (Either GenError ())
  run engine = do
    pure . Left . SimpleMsg $ pack (show engine) <> " not implemented."

class Context contextT where
  findItem :: contextT -> Text -> Maybe FileItem
  findItem _ _ = Nothing

class Show workitemT => WorkItem workitemT where
  process :: workitemT -> IO (Either GenError ())
  process workItem = do
    pure . Left . SimpleMsg $ pack (show workItem) <> " process not implemented."

data (Engine eT, Context cT, WorkItem wiT) => WorkPlan eT cT wiT =
  WorkPlan {
    -- TODO: add a 'project' field to hold the project definition -> context for execution with params for
    -- driving the logic and supplying data in VM exec context.
    destDir :: FilePath
    , items :: [ wiT ]
    , engine :: eT
    , context :: cT
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


instance WorkItem ScfWorkItem where
  process = \case
    NewDirIfNotExist dirPath -> do
      putStrLn $ "@[runItem] NewDirIfNotExist: " <> dirPath
      pure . Left . SimpleMsg $ "NewDirIfNotExist not implemented."
    DupFromSource fItem srcPath destPath -> do
      putStrLn $ "@[runItem] DupFromSource: " <> srcPath <> " -> " <> destPath
      pure . Left . SimpleMsg $ "DupFromSource not implemented."
    CloneSource srcPath destPath -> do
      putStrLn $ "@[runItem] CloneSource: " <> srcPath <> " -> " <> destPath
      pure . Left . SimpleMsg $ "CloneSource not implemented."
    RunTemplate path -> do
      putStrLn $ "@[runItem] RunTemplate: " <> path
      pure . Left . SimpleMsg $ "RunTemplate not implemented."
    RunTemplateToDest kind dir src dest -> do
      putStrLn $ "@[runItem] RunTemplateToDest: " <> show kind <> " " <> dir <> " " <> show src <> " " <> dest
      pure . Left . SimpleMsg $ "RunTemplateToDest not implemented."
    ConfigWith fItem -> do
      putStrLn $ "@[runItem] ConfigWith: " <> show fItem
      pure . Left . SimpleMsg $ "ConfigWith not implemented."
