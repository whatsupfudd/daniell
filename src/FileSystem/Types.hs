module FileSystem.Types where

import qualified Data.Sequence as Seq
import qualified Data.Map as Mp


data FileItem =
  KnownFile FileKind FilePath
  | MiscFile FilePath
  deriving Show

-- TODO: group certain files by category (yaml/toml/json, markdown/rss/pandoc/asciidoc, etc)
data FileKind =
  Html
  | Yaml
  | Toml
  | Json
  | DanTmpl
  | Haskell
  | Elm
  | Typescript
  | Javascript
  | TsReact
  | JsReact
  | EmacsOrg
  | Css
  | Rss
  | AsciiDoc
  | Markdown
  | Pandoc
  | Xml
  | TxtTempl
  deriving (Eq, Ord, Show)


getItemPath :: FileItem -> FilePath
getItemPath (MiscFile fp) = fp
getItemPath (KnownFile _ fp) = fp


type PathNode = (FilePath, [FileItem])
type PathFiles = Seq.Seq PathNode
type FileWithPath = (FilePath, FileItem)

type DirTreeMap = Mp.Map FilePath DirNode

data DirNode = DirNode {
    dirPath :: FilePath
    , subTree :: DirTreeMap
    , files :: [ FileItem ]
  }
