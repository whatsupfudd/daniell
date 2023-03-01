module Markup.Page where

import Conclusion (GenError (..))
import Options.RunOptions
import Markup.Types


parseContent :: RunOptions -> FilePath -> IO (Either GenError MarkupPage)
parseContent rtOpts contentPath =
  -- load the file
  -- detect the content format (markdown, ?)
  -- send to the right parser.
  -- build page variables accordding to FrontMatter (into FrontMatter?)
  pure . Left $ SimpleMsg "@[parseContent] TODO!"