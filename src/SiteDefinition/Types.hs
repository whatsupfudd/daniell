module SiteDefinition.Types where


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


data SiteEntry a = SiteEntry {
    path :: FilePath
    deref :: Maybe a
  }

data SiteVarMap = Map Text SiteCVarTree


data SiteVarTree =
  TermSO SiteVar
  | NodeSO SiteVar [ SiteVarTree ]


data SiteDefinition = SiteDefinition {
    baseDir :: String
    , markupContent :: [ SiteEntry ]
    , themes :: [ SiteEntry ]
    , templates :: [ SiteEntry ]
    , assets :: [ SiteEntry ]
    , dataSources :: [ SiteEntry ]
    , resources :: [ SiteEntry ]
    , staticDest :: FilePath
    , configs :: [ SiteEntry ]
  }

