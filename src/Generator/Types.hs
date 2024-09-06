module Generator.Types where

import FileSystem.Types (FileItem, FileKind)

data WorkPlan =
  WorkPlan {
    -- TODO: add a 'project' field to hold the project definition -> context for execution with params for
    -- driving the logic and supplying data in VM exec context.
    destDir :: FilePath
    , items :: [ WorkItem ]
  }
  deriving Show

data WorkItem =
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
