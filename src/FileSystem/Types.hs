module FileSystem.Types where

import qualified Data.Sequence as Seq
import qualified Data.Map as Mp


data FileItem =
  KnownFile FileKind FilePath
  | MiscFile FilePath
  deriving Show

data FileKind =
  Markup
  | Html
  | Yaml
  | Toml
  | Json
  | Haskell
  | Elm
  | DanTmpl
  | TsReact
  | JsReact
  deriving (Eq, Show)


getItemPath :: FileItem -> FilePath
getItemPath (MiscFile fp) = fp
getItemPath (KnownFile _ fp) = fp


type PathNode = (FilePath, [FileItem])
type PathFiles = Seq.Seq PathNode

type DirTreeMap = Mp.Map FilePath DirNode

data DirNode = DirNode {
    dirPath :: FilePath
    , subTree :: DirTreeMap
    , files :: [ FileItem ]
  }
