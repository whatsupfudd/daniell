{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module Markup.Markdown where

import qualified Data.Map as Mp
import qualified Data.HashMap.Lazy as Hl
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, breakOn)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE

import qualified System.FilePath as Sfp

import qualified Text.MMark as Mmkp
import qualified Text.Megaparsec as Prsc
import qualified Lucid as Lcd

import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Ae
import qualified Toml as Tm

import Conclusion (GenError (..))

import Markup.Types
import Data.Char (isSpace)
import Data.Aeson (Value(Bool))
import GHC.Generics (Generic)

{-
TOML : identified by opening and closing +++.
YAML : identified by opening and closing ---.
JSON : a single JSON object surrounded by ‘{’ and ‘}’, followed by a new line.
ORG : a group of Org mode keywords in the format ‘#+KEY: VALUE’. Any line that does not start with #+ ends the front matter section. Keyword values can be either strings (#+KEY: VALUE) or a whitespace separated list of strings (#+KEY[]: VALUE_1 VALUE_2).
-}


parse :: FilePath -> IO (Either GenError MarkupPage)
parse filePath = do
  let
    fileName = Sfp.takeBaseName filePath
    -- TODO: create real logic:
    outPath = Sfp.addExtension (Sfp.joinPath ["/tmp", fileName]) "html"
  txt <- T.readFile filePath
  case parseFrontMatter txt of
    Left errMsg -> do
      putStrLn $ "@[parse] parseFrontMatter err: " <> errMsg
      pure . Left . SimpleMsg $ pack errMsg
    Right (mbFrontMatter, rest) -> do
      -- TODO: do real stuff:
      -- putStrLn $ "@[parse] parseFrontMatter: " <> show mbFrontMatter
      pure . Right $ MarkupPage {
                path = filePath
              , frontMatter = mbFrontMatter
              , content = Content RawText (Just rest)
            }

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


data YamlValue =
  BoolYV Bool
  | IntYV Int
  | DoubleYV Double
  | TextYV Text
  | ArrayYV [YamlValue]
  | ObjectYV (Mp.Map Text YamlValue)
  deriving (Show, Generic)

{-
instance Ae.FromJSON YamlValue where
  parseJSON (Ae.Object o) = 
-}  


parseFrontMatter :: Text -> Either String (Maybe FrontMatter, Text)
parseFrontMatter aText =
  let
    cleanText = T.stripStart aText
    peekFormat
      | T.isPrefixOf "---" cleanText = (YamlEnc, T.dropWhile (\c -> c == '-' || isSpace c) cleanText)
      | T.isPrefixOf "+++" cleanText = (TomlEnc, T.dropWhile (\c -> c == '-' || isSpace c) cleanText)
      | T.isPrefixOf "{" cleanText = (JsonEnc, T.drop 1 $ T.stripStart aText)
      | T.isPrefixOf "#" cleanText = (OrgEnc, cleanText)
      | otherwise = (UnknownEnc, cleanText)
  in
  parseFrontMatterFrom peekFormat
  where
  parseFrontMatterFrom :: (FMEncoding, Text) -> Either String (Maybe FrontMatter, Text)
  parseFrontMatterFrom (aKind, aText) =
    case aKind of
      YamlEnc ->
        let
          (fmText, rest) = T.breakOn "\n---" aText
        in
        -- TODO: map the content of the Yaml data to the internal representation.
        case Yaml.decodeEither' . TE.encodeUtf8 $ fmText :: Either Yaml.ParseException (Hl.HashMap Text Ae.Value) of
          Left err -> Left $ "@[parseFrontMatterFrom] Yaml.decodeEither' err: " <> show err
          Right aMap ->
            let
              rez = Hl.toList aMap
              -- TODO: do a real conversion!!
              fmDec = FrontMatter YamlEnc (Mp.fromList $ fmap (\(k, v) -> (k, ValueDF v)) rez)
            in
              Right (Just fmDec, T.dropWhile (== '\n') . T.dropWhile (== '-') $ rest)
      TomlEnc ->
        let
          (fmText, rest) = breakOn "\n+++" aText
        in Left "gaga"
      JsonEnc ->
        let
          (fmText, rest) = breakOn "\n}\n" aText
        in Left "gaga"
      OrgEnc ->
        -- TODO: Find first line that doesn't start with a '#+'
        Left "@[parseFrontMatterFrom] unimplemented front-matter: org."
      UnknownEnc -> Left "@[parseFrontMatterFrom] unknown encoding."

