module Markup.Types where

import qualified Data.Map as Mp
import Data.Text (Text)

{- TODO:
 * FrontMatter: add encoding style (toml / yaml / json)
 * Remove GenFormat, it's not part of the information in Markup doc
 * Update Definition with things that actually make sense.
 * Add origin of content (filepath (full dir? just filename?)).
-}


data FieldsSyntax =
  TomlFT
  | YamlFT
  | JsonFT


data FrontMatter = FrontMatter {
    fields :: Mp.Map Text Definition
    , syntax :: FieldsSyntax
  }

type Content = Text

data Encoding =
  MarkdownEC

data Definition =
  ValueDF
  | EnvVarDF
  | ReferenceDF


data MarkupPage =
  MkPage Encoding FrontMatter Content
