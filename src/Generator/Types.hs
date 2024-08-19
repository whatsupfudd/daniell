module Generator.Types where

import FileSystem.Types (FileItem, FileKind)


data WorkItem =
  NewDirIfNotExist FilePath
  -- parse source, exec and save result to destination, based on: source type, source location, destination location
  | DupFromSource FileItem FilePath FilePath
  -- straight copy from source to destination:
  | CloneSource FilePath FilePath
  -- run a 'logic' template, no implied result.AnonymousRoutes
  | RunTemplate FilePath
  | RunTemplateToDest FileKind FilePath FileItem FilePath
  deriving Show
