module SiteDefinition.Types where

import Data.Text (Text)

{-
 TODO:
 * Add:
  - markup content
  - themes (layouts + support data to implement a look)
  - templates (rendering sources (html, xml, ...))
  - assets
  - data sources
  - static resources + ?destination?
  - other resources
  - global info context (extracted from config files?)

 * Create:
  - WorkPlan: description of what needs to be done to each (MarkupPage, [Templates]) (runtime engine, parameters).
  - Transformation: description of what needs to be done to output the content in the right format.
-}


newtype TmpFileDef = TmpFileDef { aPath :: FilePath }


data SiteEntry a = SiteEntry {
    path :: FilePath
    , deref :: Maybe a
  }

data SiteVarMap a = Map Text (SiteVarTree a)


data SiteVarTree a =
  TermSO (SiteEntry a)
  | NodeSO (SiteEntry a) [ SiteVarTree a ]


data SiteDefinition a = SiteDefinition {
    baseDir :: String
    , markupContent :: [ SiteEntry a ]
    , themes :: [ SiteEntry a ]
    , templates :: [ SiteEntry a ]
    , assets :: [ SiteEntry a ]
    , dataSources :: [ SiteEntry a ]
    , resources :: [ SiteEntry a ]
    , staticDest :: FilePath
    , configs :: [ SiteEntry a ]
  }

