module Options.Runtime where

import Options.SiteConfig as Scfg
import WebServer.CorsPolicy (CORSConfig, defaultCorsPolicy)

data RunOptions = RunOptions {
    debug :: Int
    , siteConf :: Scfg.Config
    , baseDir :: FilePath
    , jwkConfFile :: Maybe FilePath
    , serverPort :: Int
    , corsPolicy :: Maybe CORSConfig
    , templateDir :: FilePath
  }

defaultRun appHome baseURL =
  RunOptions {
    debug = 0
    , siteConf = Scfg.defaultConfig baseURL
    , baseDir = "/Volumes/Ts220821/Documents/Projets/Fudd/ASite"
    , jwkConfFile = Just $ appHome <> "/jwkConf.json"
    , serverPort = 7885
    , corsPolicy = Just defaultCorsPolicy
    , templateDir = appHome <> "/templates"
  }