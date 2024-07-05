module Options  (
  module Options.Cli
  , module Fo
  , module Rt
  , EnvOptions (..)
  , mergeOptions
 )
where

import Control.Monad.State ( MonadState, runState, State, modify )

import Data.Text (Text, unpack, pack)
import Data.Maybe (maybe, isJust, isNothing)

import Options.Cli
import Options.ConfFile as Fo
import qualified Options.Runtime as Rt ( RunOptions (..), defaultRun )
import WebServer.CorsPolicy ( CORSConfig(..), defaultCorsPolicy )

data EnvOptions = EnvOptions {
    danHome :: Maybe Text
    , listenPort :: Maybe Int
  }

type RunOptSt = State Rt.RunOptions (Either String ())
mconf :: MonadState s m => Maybe t -> (t -> s -> s) -> m ()
mconf mbOpt setter =
  case mbOpt of
    Nothing -> pure ()
    Just aVal -> modify $ setter aVal

-- | mergeOptions gives priority to CLI options, followed by config-file options, followed
--   by environment variables.
mergeOptions :: CliOptions -> Fo.FileOptions -> EnvOptions -> Rt.RunOptions
mergeOptions cli file env =
  let
    (result, runtimeOpts) = runState (parseOptions cli file) (Rt.defaultRun "http://localhost")
  in
  case result of
    Left errMsg -> error errMsg
    Right _ -> runtimeOpts
  where
    parseOptions :: CliOptions -> Fo.FileOptions -> RunOptSt
    parseOptions cli file = do
      mconf cli.debug $ \nVal s -> s { Rt.debug = nVal }
      case file.server of
        Nothing -> pure . Right $ ()
        Just so -> parseServer so
      case file.jwt of
        Nothing -> pure . Right $ ()
        Just jo -> parseJWT jo
      pure . Right $ ()

    parseServer :: ServerOpts -> RunOptSt
    parseServer so = do
      mconf so.port $ \nVal s -> s { Rt.serverPort = nVal }
      -- TODO: parse cache
      pure . Right $ ()

    parseJWT :: JwtOpts -> RunOptSt
    parseJWT jo = do
      case jo.jEnabled of
        Just False ->
          modify $ \s -> s { Rt.jwkConfFile = Nothing }
        _ ->
          mconf jo.keyFile $ \nVal s -> s { Rt.jwkConfFile = Just nVal }
      pure . Right $ ()

    parseCors :: CorsOpts -> RunOptSt
    parseCors co = do
      case co.oEnabled of
        Just False ->
          modify $ \s -> s { Rt.corsPolicy = Nothing }
        _ ->
          mconf co.allowed $ \nVal s -> s { Rt.corsPolicy = Just $ defaultCorsPolicy { allowedOrigins = map pack nVal } }
      pure . Right $ ()

