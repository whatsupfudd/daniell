{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module Markup.FrontMatter where

import Data.Char (isSpace)
import qualified Data.HashMap.Lazy as Hl
import qualified Data.Map as Mp
import Data.Text (Text, breakOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Ae
import qualified Toml as Tm

import Data.Aeson (Value(Bool))
import GHC.Generics (Generic)

import ProjectDefinition.Types (DictEntry)
import Markup.Types

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
          (fmText, rest) = T.breakOn "---\n" aText
        in
        -- TODO: map the content of the Yaml data to the internal representation.
        case Yaml.decodeEither' . TE.encodeUtf8 $ fmText :: Either Yaml.ParseException (Hl.HashMap Text DictEntry) of
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
          (fmText, rest) = breakOn "+++\n" aText
        in Left "gaga"
      JsonEnc ->
        let
          (fmText, rest) = breakOn "\n}\n" aText
        in Left "gaga"
      OrgEnc ->
        -- TODO: Find first line that doesn't start with a '#+'
        Left "@[parseFrontMatterFrom] unimplemented front-matter: org."
      UnknownEnc -> Left "@[parseFrontMatterFrom] unknown encoding."

