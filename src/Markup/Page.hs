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
    ".org" -> pure . Left . SimpleMsg $ "@[parseContent] emacs org content not supported: " <> pack filePath
    ".rss" -> pure . Left . SimpleMsg $ "@[parseContent] rss content not supported: " <> pack filePath
    ".pandoc" -> pure . Left . SimpleMsg $ "@[parseContent] pandoc content not supported: " <> pack filePath
    ".adoc" -> pure . Left . SimpleMsg $ "@[parseContent] asciidoc content not supported: " <> pack filePath
    ".html" -> pure . Left . SimpleMsg $ "@[parseContent] html content not supported: " <> pack filePath
    _ -> pure . Left . SimpleMsg $ "@[parseContent] unknown file extension: " <> pack filePath
