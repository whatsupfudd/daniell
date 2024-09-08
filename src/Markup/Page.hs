module Markup.Page where

import Data.Text (pack)

import Conclusion (GenError (..))
import Options.Runtime

import Markup.Markdown (parse)
import Markup.Types
import System.FilePath (takeExtension)


parseContent :: RunOptions -> FilePath -> IO (Either GenError MarkupPage)
parseContent rtOpts filePath =
  -- load the file
  -- detect the content format (markdown, ?)
  -- send to the right parser.
  -- build page variables accordding to FrontMatter (into FrontMatter?)
  case takeExtension filePath of
    ".md" -> parse filePath
    _ -> pure . Left . SimpleMsg $ "@[parseContent] unknown file extension: " <> pack filePath
