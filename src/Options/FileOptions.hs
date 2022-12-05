{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Options.FileOptions where

import qualified Control.Exception as Cexc
import Data.Aeson ((.:))
import qualified Data.Aeson as Aes
import qualified Data.Int as DI
import GHC.Generics
import qualified System.Directory as Sdir
import qualified System.FilePath.Posix as Spsx
import qualified System.IO.Error as Serr

import Toml (TomlCodec, (.=))  -- add 'TomlBiMap' and 'Key' here optionally
import qualified Toml as Toml
import qualified Validation as Vld
import qualified Data.Yaml as Yaml

import qualified Options.Config as Cnfg


data FileOptions = FileOptions {
  -- debug :: Just DI.Int32
  debug :: Int
 }
 deriving stock (Show, Generic)

instance Aes.FromJSON FileOptions


defaultConfName = ".daniell.yaml"


defaultConfigFilePath :: IO FilePath
defaultConfigFilePath = do
  eiHomeDir <- Cexc.try $ Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
  case eiHomeDir of
    Left err -> pure defaultConfName
    Right aPath -> pure $ Spsx.joinPath [aPath, defaultConfName]


tomlOptionCodec :: TomlCodec FileOptions
tomlOptionCodec = FileOptions
  <$> Toml.int "debug" .= debug


parseFileOptions :: FilePath -> IO (Either String FileOptions)
parseFileOptions filePath =
  let
    fileExt = Spsx.takeExtension filePath
  in case fileExt of
    ".yaml" -> do
      eiRez <- Yaml.decodeFileEither filePath
      case eiRez of
        Left err -> pure . Left $ "@[parseYaml] err: " <> show err
        Right aContent -> pure $ Right aContent
      {-
      eiContent <- Cnfg.parseYaml filePath
      case eiContent of
        Left errMsg -> pure . Left $ "@[parseFileOptions] err: " <> errMsg
        Right aConfig -> pure aConfig
      -}
    ".toml" -> do
      eiContent <- Cnfg.parseToml filePath
      case eiContent of
        Left err -> pure . Left $ "@[parseFileOptions] err: " <> show err
        Right aConfig ->
          case Toml.runTomlCodec tomlOptionCodec aConfig of
            Vld.Failure errs -> pure . Left $ "@[parseFileOptions] err: " <> show errs
            Vld.Success fOptions -> pure $ Right fOptions
    ".json" -> do
      eiContent <- Cnfg.parseJson filePath
      case eiContent of
        Left errMsg -> pure . Left $ "@[parseFileOptions] err: " <> errMsg
        Right aConfig -> pure aConfig
    _ -> pure . Left $ "@[parseFileOptions] unknown conf-file extension: " <> fileExt
