module Markup.Types where

import qualified Data.Map as Mp
import Data.Text (Text)

import qualified Data.Aeson as Ae
import Text.MMark (MMark)

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

data FMEncoding =
  YamlEnc
  | TomlEnc
  | JsonEnc
  | OrgEnc
  | UnknownEnc
  deriving Show


data Definition =
  ValueDF Ae.Value
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
  | RawText
  deriving Show


-- Content that fills in the template.
data MarkupPage = MarkupPage {
    path :: FilePath
    , frontMatter :: Maybe FrontMatter
    , content :: Content
  }
  deriving Show
