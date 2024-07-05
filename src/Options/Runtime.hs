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
  }

defaultRun baseURL =
  RunOptions {
    debug = 0
    , siteConf = Scfg.defaultConfig baseURL
    , baseDir = "/Volumes/Ts220821/Documents/Projets/Fudd/ASite"
    , jwkConfFile = Just "/Users/lhugo/.fudd/daniell/jwkConf.json"
    , serverPort = 7885
    , corsPolicy = Just defaultCorsPolicy
  }