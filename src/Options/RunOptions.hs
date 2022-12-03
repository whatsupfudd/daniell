module Options.RunOptions where

import Data.Int (Int)

import Options.SiteConfig as Scfg
import WebServer.CorsPolicy (CORSConfig, defaultCorsPolicy)

data RunOptions = RunOptions {
    siteConf :: Scfg.Config
    , baseDir :: FilePath
    , jwkConfFile :: FilePath
    , serverPort :: Int
    , corsPolicy :: CORSConfig
  }

defaultRun baseURL =
  RunOptions {
    siteConf = Scfg.defaultConfig baseURL
    , baseDir = "/Volumes/Ts220821/Documents/Projets/Fudd/ASite"
    , jwkConfFile = "/Users/lhugo/.daniell/jwkConf.json"
    , serverPort = 7885
    , corsPolicy = defaultCorsPolicy
  }