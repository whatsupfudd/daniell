module ProjectDefinition.NextJS where

import ProjectDefinition.Types


defaultNextJS :: NextJSComponents
defaultNextJS = NextJSComponents {
    config = NextJSConfig {
        envConfig = []
        , nextConfig = []
        , packageConfig = []
        , tsConfig = []
      }
    , components = []
    , pages = []
    , api = []
    , lib = []
    , styles = []
    , utils = []
    , hooks = []
    , services = []
    , types = []
    , tests = []
    , stories = []
    , public = []
    , build = []
    , deploy = []
  }
