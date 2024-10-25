module Markup.Types where

import qualified Data.Map as Mp
import Data.Text (Text)

import Text.MMark (MMark)

import FileSystem.Types (FileWithPath)
import ProjectDefinition.Types (DictEntry)

{- TODO:
 * FrontMatter: add encoding style (toml / yaml / json)
 * Remove GenFormat, it's not part of the information in Markup doc
 * Update Definition with things that actually make sense.
 * Add origin of content (filepath (full dir? just filename?)).
-}


data FrontMatter = FrontMatter {
    encoding :: FMEncoding
    , fields :: Mp.Map Text Definition
  }
  deriving Show


-- TODO: figure out is Hugo is indeed using OrgEnc for front-matter (mentioned in content section, but not in front-matter section).
data FMEncoding =
  YamlEnc
  | TomlEnc
  | JsonEnc
  | OrgEnc
  | UnknownEnc
  deriving Show


data Definition =
  ValueDF DictEntry
  | EnvVarDF
  | ReferenceDF
  deriving Show


data Content = Content {
    encoding :: ContentEncoding
    , body :: Maybe Text
  }
  deriving Show

data ContentEncoding =
  ParsedMarkdown MMark
  | RawMarkdown
  | RawHtml
  deriving Show


-- Content that fills in the template.
data MarkupPage = MarkupPage {
    item :: FileWithPath
    , frontMatter :: Maybe FrontMatter
    , content :: Content
  }
  deriving Show
