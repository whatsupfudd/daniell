module Markup.Markdown where

import Data.Text (Text)
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import qualified Data.Text.Lazy.IO as TL
import qualified Text.MMark as Mmkp
import qualified Text.Megaparsec as Prsc
import qualified Lucid as Lcd
import qualified System.FilePath as Sfp


{-
TOML : identified by opening and closing +++.
YAML : identified by opening and closing ---.
JSON : a single JSON object surrounded by ‘{’ and ‘}’, followed by a new line.
ORG : a group of Org mode keywords in the format ‘#+KEY: VALUE’. Any line that does not start with #+ ends the front matter section. Keyword values can be either strings (#+KEY: VALUE) or a whitespace separated list of strings (#+KEY[]: VALUE_1 VALUE_2).
-}

data FrontMatterEncoding = YamlEnc | TomlEnc | JsonEnc | OrgEnc | UnknownEnc


parse :: FilePath -> IO ()
parse filePath = do
  let
    fileName = Sfp.takeBaseName filePath
    outPath = Sfp.addExtension (Sfp.joinPath ["/tmp", fileName]) "html"
  txt <- DT.readFile filePath
  (frontMatter, rest) = parseFrontMatter txt
  case Mmkp.parse filePath txt of
      Left bundle -> putStrLn (Prsc.errorBundlePretty bundle)
      Right r -> TL.writeFile outPath . Lcd.renderText . Mmkp.render $ r


parseFrontMatter aTxt =
  let 
    fmKind = case DT.head aTxt of
      '-' : YamlEnc
      '+' : TomlEnc 
      '{' : JsonEnc
      '#' : OrgEnc
      _ : UnknownEnc
  in
    parseFrontMatterFrom fmKind aTxt

parseFrontMatterFrom :: FrontMatterEncoding -> Text -> Either String (Maybe FrontMatter, Text)
parseFrontMatterFrom aKind aTxt =
  case aKind of
    UnknownEnc -> Right (Nothing, aTxt)
    YamlEnc -> -- find '---\n'
    TomlEnc -> -- find '+++\n'
    JsonEnc -> -- find '}\n'
    OrgEnc -> -- Find first line that doesn't start with a '#+'
