module Options.SiteConfig where

import Data.Text as DT

data Config = Config {
      archetypeDir :: FilePath
    , assetDir :: FilePath
    , baseURL :: Text
    -- , build ::
    , buildDrafts :: Bool
    , buildExpired :: Bool
    , buildFuture :: Bool
  }

defaultConfig baseURL =
  Config {
      archetypeDir = "archetypes"
    , assetDir = "assets"
    , baseURL = baseURL
    -- , build =
    , buildDrafts = False
    , buildExpired = False
    , buildFuture = False
  }