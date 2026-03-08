{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Options.ConfFile where

import qualified Control.Exception as Cexc
import Data.Aeson ((.:))
import qualified Data.Aeson as Aes
import qualified Data.Int as DI
import GHC.Generics
import qualified System.Directory as Sdir
import qualified System.FilePath.Posix as Spsx
import qualified System.IO.Error as Serr

import Toml (TomlCodec, (.=), dioptional)  -- add 'TomlBiMap' and 'Key' here optionally
import qualified Toml
import qualified Validation as Vld
import qualified Data.Yaml as Yaml

import qualified Options.Config as Cnfg


data ServerOpts = ServerOpts {
    lport :: Maybe Int
  , cache :: Maybe FilePath
  }
  deriving stock (Show, Generic)

data JwtOpts = JwtOpts {
    jEnabled :: Maybe Bool
  , keyFile :: Maybe FilePath
  }
  deriving stock (Show, Generic)

data CorsOpts = CorsOpts {
    oEnabled :: Maybe Bool
  , allowed :: Maybe [String]
  }
  deriving stock (Show, Generic)

data PgDbOpts = PgDbOpts {
  port :: Maybe Int
  , host :: Maybe String
  , user :: Maybe String
  , passwd :: Maybe String
  , dbase :: Maybe String
  , poolSize :: Maybe Int
  , poolTimeOut :: Maybe Int
}
  deriving stock (Show, Generic)

data FileOptions = FileOptions {
  -- debug :: Just DI.Int32
  debug :: Maybe Int
  , primaryLocale :: Maybe String
  , server :: Maybe ServerOpts
  , jwt :: Maybe JwtOpts
  , cors :: Maybe CorsOpts
  , dbPg :: Maybe PgDbOpts
 }
 deriving stock (Show, Generic)

defaultConfName = ".fudd/daniell/config.yaml"


defaultConfigFilePath :: IO FilePath
defaultConfigFilePath = do
  eiHomeDir <- Cexc.try Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
  case eiHomeDir of
    Left err -> pure defaultConfName
    Right aPath -> pure $ Spsx.joinPath [aPath, defaultConfName]


tomlOptionCodec :: TomlCodec FileOptions
tomlOptionCodec = FileOptions
  <$> dioptional (Toml.int "debug") .= debug
  <*> dioptional (Toml.string "primaryLocale") .= primaryLocale
  <*> dioptional (Toml.table serverCodec "server") .= server
  <*> dioptional (Toml.table jwtCodec "jwt") .= jwt
  <*> dioptional (Toml.table corsCodec "cors") .= cors
  <*> dioptional (Toml.table pgDbCodec "dbPg") .= dbPg


serverCodec :: TomlCodec ServerOpts
serverCodec = ServerOpts
  <$> dioptional (Toml.int "port") .= lport
  <*> dioptional (Toml.string "cache") .= cache

jwtCodec :: TomlCodec JwtOpts
jwtCodec = JwtOpts
  <$> dioptional (Toml.bool "enabled") .= jEnabled
  <*> dioptional (Toml.string "keyFile") .= keyFile

corsCodec :: TomlCodec CorsOpts
corsCodec = CorsOpts
  <$> dioptional (Toml.bool "enabled") .= oEnabled
  <*> dioptional (Toml.arrayOf Toml._String "allowed") .= allowed


pgDbCodec :: TomlCodec PgDbOpts
pgDbCodec = PgDbOpts
  <$> dioptional (Toml.int "port") .= port
  <*> dioptional (Toml.string "host") .= host
  <*> dioptional (Toml.string "user") .= user
  <*> dioptional (Toml.string "passwd") .= passwd
  <*> dioptional (Toml.string "dbase") .= dbase
  <*> dioptional (Toml.int "poolSize") .= poolSize
  <*> dioptional (Toml.int "poolTimeOut") .= poolTimeOut

-- YAML support:
instance Aes.FromJSON FileOptions
instance Aes.FromJSON ServerOpts
instance Aes.FromJSON JwtOpts
instance Aes.FromJSON CorsOpts
instance Aes.FromJSON PgDbOpts

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
