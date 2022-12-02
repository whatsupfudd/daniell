module Options.RunOptions where

import Options.SiteConfig as Scfg


data RunOptions = RunOptions {
    siteConf :: Scfg.Config
    , baseDir :: FilePath
  }

defaultRun baseURL =
  RunOptions {
    siteConf = Scfg.defaultConfig baseURL
    , baseDir = "/Volumes/Ts220821/Documents/Projets/Fudd/ASite"
  }