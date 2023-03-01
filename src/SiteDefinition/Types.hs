module SiteDefinition.Types where


{-
 TODO:
 * Add:
  - markup content
  - themes (vs ?templates? )
  - templates ( vs ?themes? )
  - assets
  - data sources
  - static resources + ?destination?
  - other resources
  - global info context (extracted from config files?)

 * Create:
  - WorkPlan: description of what needs to be done to each (MarkupPage, [Templates]) (runtime engine, parameters).
  - Transformation: description of what needs to be done to output the content in the right format.
-}


data SiteDefinition = SiteDefinition {
    baseDir :: String
  }