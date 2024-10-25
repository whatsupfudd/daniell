{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module Markup.Markdown where


import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, breakOn)
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T

import System.FilePath ((</>))

import qualified Text.MMark as Mmkp
import qualified Text.Megaparsec as Prsc
import qualified Lucid as Lcd


import Conclusion (GenError (..))

import FileSystem.Types (FileWithPath, FileItem (..), getItemPath)

import Markup.FrontMatter (parseFrontMatter)
import Markup.Types

{-
TOML : identified by opening and closing +++.
YAML : identified by opening and closing ---.
JSON : a single JSON object surrounded by ‘{’ and ‘}’, followed by a new line.
ORG : a group of Org mode keywords in the format ‘#+KEY: VALUE’. Any line that does not start with #+ ends the front matter section. Keyword values can be either strings (#+KEY: VALUE) or a whitespace separated list of strings (#+KEY[]: VALUE_1 VALUE_2).
-}


parse :: FilePath -> FileWithPath -> IO (Either GenError MarkupPage)
parse srcDir file@(dirPath, fileItem) = do
  let
    fullFilePath = srcDir </> dirPath </> getItemPath fileItem
  {-
      fileName = takeBaseName $ getItemPath filePath
      -- outPath = addExtension (joinPath ["/tmp", fileName]) "html"
  -}
  txt <- T.readFile fullFilePath
  case parseFrontMatter txt of
    Left errMsg -> do
      putStrLn $ "@[parse] parseFrontMatter err: " <> errMsg
      pure . Left . SimpleMsg $ pack errMsg
    Right (mbFrontMatter, rest) -> do
      -- TODO: do real stuff:
      -- putStrLn $ "@[parse] parseFrontMatter: " <> show mbFrontMatter
      pure . Right $ MarkupPage {
                item = file
              , frontMatter = mbFrontMatter
              , content = Content RawMarkdown (Just rest)
            }

{-
parseMarkup :: FilePath -> Maybe FrontMatter -> Text -> IO (Either GenError MarkupPage)
parseMarkup filePath mbFrontMatter rest =
      case Mmkp.parse filePath rest of
        Left bundle ->
          let
            errMsg = Prsc.errorBundlePretty bundle
          in do
          putStrLn $ "@[parse] Mmkp.parse err: " <> errMsg
          pure . Left . SimpleMsg $ pack errMsg
        Right r ->
          pure . Right $ MarkupPage {
                path = filePath
              , frontMatter = mbFrontMatter
              , content = Content (ParsedMarkdown r) (Just . L.toStrict . Lcd.renderText . Mmkp.render $ r)
            }
-}

{-
data YamlValue =
  BoolYV Bool
  | IntYV Int
  | DoubleYV Double
  | TextYV Text
  | ArrayYV [YamlValue]
  | ObjectYV (Mp.Map Text YamlValue)
  deriving (Show, Generic)

instance Ae.FromJSON YamlValue where
  parseJSON (Ae.Object o) = 
-}  
