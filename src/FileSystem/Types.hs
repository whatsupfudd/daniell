module FileSystem.Types where

import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Sequence as Seq
import qualified Data.Map as Mp


data ExtFileItem =
  ReferFI FileItem
  | ContentFI FileItem Lbs.ByteString

instance Show ExtFileItem where
  show (ReferFI fi) = show fi
  show (ContentFI fi content) = show fi <> " (" <> show (Lbs.length content) <> " bytes)"


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
  | Php
  | Python
  | Cpp
  | CppHeader
  deriving (Eq, Ord, Show)


getItemPath :: FileItem -> FilePath
getItemPath (MiscFile fp) = fp
getItemPath (KnownFile _ fp) = fp


type PathNode = (FilePath, [ExtFileItem])
type PathFiles = Seq.Seq PathNode
type FileWithPath = (FilePath, FileItem)

type DirTreeMap = Mp.Map FilePath DirNode

data DirNode = DirNode {
    dirPath :: FilePath
    , subTree :: DirTreeMap
    , files :: [ ExtFileItem ]
  }
  deriving Show
