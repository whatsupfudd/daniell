module Options.Runtime where

import Data.Text (Text)
import Options.SiteConfig as Scfg
import WebServer.CorsPolicy (CORSConfig, defaultCorsPolicy)
import Options.Types (HugoBuildOptions)

data RunOptions = RunOptions {
    debug :: Int
    , siteConf :: Scfg.Config
    , baseDir :: FilePath
    , jwkConfFile :: Maybe FilePath
    , serverPort :: Int
    , corsPolicy :: Maybe CORSConfig
    , templateDir :: FilePath
    , techOpts :: TechOptions
  }

data TechOptions =
  HugoOptions HugoBuildOptions
  | NextOptions
  | FuddleOptions
  | GatsbyOptions
  | NoTech
  deriving Show

{-
data HugoRunOptions = HugoRunOptions {
    configFiles :: Maybe Text
    , environment :: Maybe Text
    , themeName :: Maybe Text
  }
  deriving Show
-}


defaultRun :: FilePath -> Text -> RunOptions
defaultRun appHome baseURL =
  RunOptions {
    debug = 0
    , siteConf = Scfg.defaultConfig baseURL
    , baseDir = "/Volumes/Ts220821/Documents/Projets/Fudd/ASite"
    , jwkConfFile = Just $ appHome <> "/jwkConf.json"
    , serverPort = 7885
    , corsPolicy = Just defaultCorsPolicy
    , templateDir = appHome <> "/templates"
    , techOpts = NoTech
  }