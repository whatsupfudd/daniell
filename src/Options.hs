module Options  (
  module Options.Cli
  , module Fo
  , module Rt
  , EnvOptions (..)
  , mergeOptions
 )
where

import Control.Monad.State ( MonadState (put), MonadIO, runStateT, State, StateT, modify, lift, liftIO )
import Control.Monad.Except ( ExceptT, MonadError (throwError) )
import Data.Functor.Identity ( Identity (..) )

import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe (maybe, isJust, isNothing)
import Data.Foldable (for_)

import qualified System.IO.Error as Serr
import qualified Control.Exception as Cexc
import qualified System.Posix.Env as Senv
import qualified System.Directory as Sdir

import WebServer.CorsPolicy ( CORSConfig(..), defaultCorsPolicy )
import Options.Cli
import Options.ConfFile as Fo
import qualified Options.Runtime as Rt ( RunOptions (..), defaultRun )
import qualified DB.Connect as Db

data EnvOptions = EnvOptions {
    danHome :: Maybe FilePath
    , listenPort :: Maybe Int
  }


type RunOptSt = State Rt.RunOptions (Either String ())
type RunOptIOSt = StateT Rt.RunOptions IO (Either String ())
type PgDbOptIOSt = StateT Db.PgDbConfig (StateT Rt.RunOptions IO) (Either String ())

mconf :: MonadState s m => Maybe t -> (t -> s -> s) -> m ()
mconf mbOpt setter =
  case mbOpt of
    Nothing -> pure ()
    Just aVal -> modify $ setter aVal

innerConf :: MonadState s f => (t1 -> s -> s) -> (t2 -> StateT t1 f (Either a b)) -> t1 -> Maybe t2 -> f ()
innerConf updState innerParser defaultVal mbOpt =
  case mbOpt of
    Nothing -> pure ()
    Just anOpt -> do
      (result, updConf) <- runStateT (innerParser anOpt) defaultVal
      case result of
        Left errMsg -> pure ()
        Right _ -> modify $ updState updConf


-- | mergeOptions gives priority to CLI options, followed by config-file options, followed
--   by environment variables.
mergeOptions :: CliOptions -> Fo.FileOptions -> EnvOptions -> IO Rt.RunOptions
mergeOptions cli file env = do
  danHome <- case env.danHome of
    Nothing -> do
      eiHomeDir <- Cexc.try Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
      case eiHomeDir of
        Left err -> pure ".fudd/daniell"
        Right aVal -> pure $ aVal <> "/.fudd/daniell"
    Just aVal -> pure aVal
  (result, runtimeOpts) <- runStateT (parseOptions cli file) (Rt.defaultRun danHome "http://localhost")
  case result of
    Left errMsg -> error errMsg
    Right _ -> pure runtimeOpts
  where
    parseOptions :: CliOptions -> Fo.FileOptions -> RunOptIOSt
    parseOptions cli file = do
      mconf cli.debug $ \nVal s -> s { Rt.debug = nVal }
      for_ file.server parseServer
      for_ file.jwt parseJWT
      for_ file.cors parseCors
      innerConf (\nVal s -> s { Rt.pgDbConf = nVal }) parsePgDb Db.defaultPgDbConf file.dbPg
      pure $ Right ()


    parseServer :: ServerOpts -> RunOptIOSt
    parseServer so = do
      mconf so.lport $ \nVal s -> s { Rt.serverPort = nVal }
      -- TODO: parse cache
      pure $ Right ()

    parseJWT :: JwtOpts -> RunOptIOSt
    parseJWT jo = do
      case jo.jEnabled of
        Just False -> do
          modify $ \s -> s { Rt.jwkConfFile = Nothing }
          pure $ Right ()
        _ ->
          case jo.keyFile of
            Nothing -> pure $ Right ()
            Just aPath -> do
              mbJwkPath <- liftIO $ resolveEnvValue aPath
              case mbJwkPath of
                Nothing ->
                  pure . Left $ "Could not resolve JWK file path: " <> aPath
                Just aPath -> do
                  modify $ \s -> s { Rt.jwkConfFile = Just aPath }
                  pure $ Right ()

    parseCors :: CorsOpts -> RunOptIOSt
    parseCors co = do
      case co.oEnabled of
        Just False ->
          modify $ \s -> s { Rt.corsPolicy = Nothing }
        _ ->
          mconf co.allowed $ \nVal s -> s { Rt.corsPolicy = Just $ defaultCorsPolicy { allowedOrigins = map pack nVal } }
      pure $ Right ()

    parsePgDb :: Fo.PgDbOpts -> PgDbOptIOSt
    parsePgDb dbO = do
      mconf dbO.host $ \nVal s -> s { Db.host = T.encodeUtf8 . T.pack $ nVal }
      mconf dbO.port $ \nVal s -> s { Db.port = fromIntegral nVal }
      mconf dbO.user $ \nVal s -> s { Db.user = T.encodeUtf8 . T.pack $ nVal }
      mconf dbO.passwd $ \nVal s -> s { Db.passwd = T.encodeUtf8 . T.pack $ nVal }
      mconf dbO.dbase $ \nVal s -> s { Db.dbase = T.encodeUtf8 . T.pack $ nVal }
      pure $ Right ()


-- | resolveEnvValue resolves an environment variable value.
resolveEnvValue :: FilePath -> IO (Maybe FilePath)
resolveEnvValue aVal =
  case head aVal of
      '$' ->
        let
          (envName, leftOver) = break ('/' ==) aVal
        in do
        mbEnvValue <- Senv.getEnv $ tail envName
        case mbEnvValue of
          Nothing -> pure Nothing
          Just aVal -> pure . Just $ aVal <> leftOver
      _ -> pure $ Just aVal
