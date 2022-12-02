module Options.RunOptions where

import Options.SiteConfig as Scfg


data RunOptions = RunOptions {
    siteConf :: Scfg.Config
  }

defaultRun baseURL =
  RunOptions {
    siteConf = Scfg.defaultConfig baseURL
  }