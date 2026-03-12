module FileSystem.Types where

import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Sequence as Seq
import qualified Data.Map as Mp
import Data.Text (Text)


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
    AsciiDoc
  | Cpp
  | CppHeader
  | Css
  | DanTmpl
  | Elm
  | EmacsOrg
  | Haskell
  | Html
  | Javascript
  | Json
  | JsReact
  | Markdown
  | Pandoc
  | Php
  | Python
  | Rss
  | RubyFK
  | Toml
  | TsReact
  | TxtTempl
  | Typescript
  | Xml
  | Yaml
  deriving (Eq, Ord, Show)


fileKindToStr :: FileKind -> Text
fileKindToStr Html = "html"
fileKindToStr Javascript = "js"
fileKindToStr Php = "php"
fileKindToStr _ = "unknown"


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


getExtFileItemPath :: ExtFileItem -> FilePath
getExtFileItemPath (ReferFI fileItem) = getItemPath fileItem
getExtFileItemPath (ContentFI fileItem _) = getItemPath fileItem

getExtFileItemKind :: ExtFileItem -> Maybe FileKind
getExtFileItemKind (ReferFI fileItem) = getItemKind fileItem
getExtFileItemKind (ContentFI fileItem _) = getItemKind fileItem

getItemKind :: FileItem -> Maybe FileKind
getItemKind (MiscFile _) = Nothing
getItemKind (KnownFile fileKind _) = Just fileKind


isHtmlExtItem :: ExtFileItem -> Bool
isHtmlExtItem (ReferFI fileItem) = isHtmlFileItem fileItem
isHtmlExtItem (ContentFI fileItem _) = isHtmlFileItem fileItem

isHtmlFileItem :: FileItem -> Bool
isHtmlFileItem (MiscFile filePath) = False
isHtmlFileItem (KnownFile fileKind _) = fileKind == Html

isJsExtItem :: ExtFileItem -> Bool
isJsExtItem (ReferFI fileItem) = isJsFileItem fileItem
isJsExtItem (ContentFI fileItem _) = isJsFileItem fileItem

isJsFileItem :: FileItem -> Bool
isJsFileItem (MiscFile filePath) = False
isJsFileItem (KnownFile fileKind _) = fileKind == Javascript

isPhpExtItem :: ExtFileItem -> Bool
isPhpExtItem (ReferFI fileItem) = isPhpFileItem fileItem
isPhpExtItem (ContentFI fileItem _) = isPhpFileItem fileItem

isPhpFileItem :: FileItem -> Bool
isPhpFileItem (MiscFile filePath) = False
isPhpFileItem (KnownFile fileKind _) = fileKind == Php

isRubyExtItem :: ExtFileItem -> Bool
isRubyExtItem (ReferFI fileItem) = isRubyFileItem fileItem
isRubyExtItem (ContentFI fileItem _) = isRubyFileItem fileItem

isRubyFileItem :: FileItem -> Bool
isRubyFileItem (MiscFile filePath) = False
isRubyFileItem (KnownFile fileKind _) = fileKind == RubyFK

