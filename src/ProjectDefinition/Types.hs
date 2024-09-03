{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module ProjectDefinition.Types where

import Data.Text (Text)
import Data.Type.Coercion (TestCoercion)
import Template.Types (ScaffholdTempl, FileTempl)
import FileSystem.Types (PathFiles)


{-
Note:
 * The project sub-components are part of the ProjectType. For a Hugo subtype, there will be:
  - markup content
  - themes (layouts + support data to implement a look)
  - templates (rendering sources (html, xml, ...))
  - assets
  - data sources
  - static resources + ?destination?
  - other resources
  - global info context (extracted from config files?)
-}
data ProjectDefinition = ProjectDefinition {
    baseDir :: FilePath
    , pType :: ProjectType
    , templates :: [ ScaffholdTempl ]
    , sourceContent :: PathFiles
  }

defaultProjectDef :: FilePath -> String -> ProjectType -> ProjectDefinition
defaultProjectDef baseDir pName pT = ProjectDefinition {
    baseDir = baseDir
    , pType = pT
    , templates = []
    , sourceContent = mempty
   }


data ProjectType =
  Site SiteType
  | WebApp WebAppType
  | LocalApp LocalAppType

data SiteType =
  Hugo HugoComponents
  | WordPress WordPressComponents

data WebAppType =
  NextJS NextJSComponents
  | Fuddle FuddleComponents

data LocalAppType =
  FuddApp LocalAppComponents


{- FUDDAPP -}
data LocalAppComponents = LocalAppComponents {
    config :: String
    , components :: [ TemplItem ]
  }

{- FUDDLE -}
data FuddleComponents = FuddleComponents {
    config :: String
    , components :: [ TemplItem ]
  }

{- HUGO -}
data HugoComponents = HugoComponents {
    markupContent :: [ TemplItem ]
    , themes :: [ TemplItem ]
    , templates :: [ ProjEntry ]
    , assets :: [ ProjEntry ]
    , dataSources :: [ ProjEntry ]
    , resources :: [ ProjEntry ]
    , configs :: [ TemplItem ]
    , staticDest :: FilePath
}

{- WORDPRESS -}
data WordPressComponents = WordPressComponents {
    dbConfig :: DbConfig
    , initValues :: [ String ]
  }
 
data DbConfig = DbConfig {
    dbHost :: String
    , dbPort :: Int
    , dbName :: String
    , dbUser :: String
    , dbPass :: String
  }

{- NextJS -}
data NextJSComponents = NextJSComponents {
      config :: NextJSConfig
      , components :: [ TemplItem ]
      , pages :: [ TemplItem ]
      , api :: [ TemplItem ]
      , lib :: [ TemplItem ]
      , styles :: [ TemplItem ]
      , utils :: [ TemplItem ]
      , hooks :: [ TemplItem ]
      , services :: [ TemplItem ]
      , types :: [ TemplItem ]
      , tests :: [ TemplItem ]
      , stories :: [ TemplItem ]
      , public :: [ TemplItem ]
      , build :: [ TemplItem ]
      , deploy :: [ TemplItem ]
    }

data NextJSConfig = NextJSConfig {
    envConfig :: [ String ]
    , nextConfig :: [ String ]
    , packageConfig :: [ String ]
    , tsConfig :: [ String ]
  }

{- TODO: Figure out what goes in the components of each project definition. -}
data TemplItem

{- Old Stuff -}

newtype TmpFileDef = TmpFileDef { aPath :: FilePath }


data ProjEntry = ProjEntry {
    path :: FilePath
  }

data SiteVarMap a = Map Text (SiteVarTree a)


data SiteVarTree a =
  TermSO ProjEntry
  | NodeSO ProjEntry [ SiteVarTree a ]

