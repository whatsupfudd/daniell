{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module ProjectDefinition.Types where

import qualified Data.Map.Strict as Mp
import Data.Text (Text)
import Data.Type.Coercion (TestCoercion)
import qualified Data.Vector as Vc

import qualified Data.Aeson.Types as Ae
import Data.Scientific (toRealFloat)

import Template.Types (ScaffoldTempl, FileTempl)
import FileSystem.Types (PathFiles, FileItem, FileWithPath, FileKind)



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
    , templates :: [ ScaffoldTempl ]
    , sourceContent :: PathFiles
  }


{- TODO: remove if not used.
defaultProjectDef :: FilePath -> String -> ProjectType -> ProjectDefinition
defaultProjectDef baseDir pName pT = ProjectDefinition {
    baseDir = baseDir
    , pType = pT
    , templates = []
    , sourceContent = mempty
   }
-}


data ProjectType =
  Site SiteType
  | WebApp WebAppType
  | LocalApp LocalAppType

data SiteType =
  Hugo
  | NextStatic
  | WordPress

data WebAppType =
  NextJS
  | Fuddle

data LocalAppType =
  FuddApp


-- TODO: Move these definitions to their respective modules.
{- FUDDAPP -}
data LocalAppComponents = LocalAppComponents {
    config :: String
    , components :: [ TmpItem ]
  }

{- FUDDLE -}
data FuddleComponents = FuddleComponents {
    config :: String
    , components :: [ TmpItem ]
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

{- MISC structures -}
type FileSet = (Mp.Map FileKind [FileWithPath], [FileWithPath])
type OrgMap = Mp.Map String [FileWithPath]


data DictEntry =
  StringDV Text
  | IntDV Integer
  | DoubleDV Double
  | BoolDV Bool
  | DictDV (Mp.Map Text DictEntry)
  | ListDV [ DictEntry ]
  deriving Show


instance Ae.FromJSON DictEntry where
  parseJSON val =
    case val of
      Ae.Array anArray -> ListDV <$> mapM Ae.parseJSON (Vc.toList anArray)
      Ae.String s -> pure $ StringDV s
      Ae.Number n -> pure $ DoubleDV $ toRealFloat n
      Ae.Bool b -> pure $ BoolDV b
      Ae.Null -> pure $ StringDV ""
      Ae.Object obj -> -- DictDV Mp.fromList <$> mapM (\(key, value) -> pure (pack . show $ key, value)) (Ae.toList obj)
        DictDV <$> Ae.parseJSON val



{- TODO: Figure out what goes in the components of each project definition. -}
data TmpItem

