module ProjectDefinition.Hugo where

import ProjectDefinition.Types (HugoComponents (..))

{- For Hugo project, use archetype/* to create a new document in the content section -}

defaultComponents :: HugoComponents
defaultComponents = HugoComponents {
    markupContent = []
    , themes = []
    , templates = []
    , assets = []
    , dataSources = []
    , resources = []
    , configs = []
    , staticDest = "public"
  }

