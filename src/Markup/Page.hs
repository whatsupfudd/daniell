module Markup.Page where

import Data.Text (pack)

import Conclusion (GenError (..))
import Options.Runtime
import FileSystem.Types (FileWithPath, getItemPath)

import qualified Markup.Markdown as Mkd
import qualified Markup.Html as Ht
import Markup.Types
import System.FilePath (takeExtension)


parseContent :: RunOptions -> FilePath -> FileWithPath -> IO (Either GenError MarkupPage)
parseContent rtOpts srcDir file@(dirPath, fileItem) =
  -- load the file
  -- detect the content format (markdown, ?)
  -- send to the right parser.
  -- build page variables accordding to FrontMatter (into FrontMatter?)
  case takeExtension $ getItemPath fileItem of
    ".md" -> Mkd.parse srcDir file
    ".html" -> Ht.parse srcDir file
    ".org" -> pure . Left . SimpleMsg $ "@[parseContent] emacs org content not supported: " <> pack (show file)
    ".rss" -> pure . Left . SimpleMsg $ "@[parseContent] rss content not supported: " <> pack (show file)
    ".pandoc" -> pure . Left . SimpleMsg $ "@[parseContent] pandoc content not supported: " <> pack (show file)
    ".adoc" -> pure . Left . SimpleMsg $ "@[parseContent] asciidoc content not supported: " <> pack (show file)
    _ -> pure . Left . SimpleMsg $ "@[parseContent] unknown file extension: " <> pack (show file)
