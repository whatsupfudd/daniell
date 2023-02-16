module Markup.ContentGen where

data GenFormat =
  HtmlGF
  | AmpGF
  | XmlGF


data ContentGen =
  Markdown GenFormat FrontMatter Content


parseContent :: RunOptions -> FilePath -> IO (Either GenError ContentGen)
parseContent rtOpts contentPath =
  -- load the file
  -- detect the content format (markdown, ?)
  -- send to the right parser.
  -- build page variables accordding to FrontMatter (into FrontMatter?)
  pure $ Left "@[parseContent] TODO!"