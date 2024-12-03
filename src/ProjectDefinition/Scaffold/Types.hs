module ProjectDefinition.Scaffold.Types where


import FileSystem.Types (FileItem, FileKind)
import Scaffold.Types (ScaffoldBundle (..))
import Generator.Types (WorkPlan (..))


type ScfWorkPlan = WorkPlan ScfEngine ScfContext ScfWorkItem

data ScfEngine = ScfEngine
  deriving Show

data ScfContext = ScfContext {
    bundle :: ScaffoldBundle
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
