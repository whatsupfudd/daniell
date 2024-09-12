module ProjectDefinition.Hugo.Types where

import Data.ByteString (ByteString)
import Data.Map (Map, empty)
import Data.Text (Text)

import FileSystem.Types (FileWithPath)
import ProjectDefinition.Types (DictEntry)

-- Analysis context for a Hugo project (configurations, etc) that will be used to drive generation.
data AnalyzeContext = AnalyzeContext {
    globalVars :: Map Text DictEntry
    , defaultVars :: Map Text DictEntry
    , otherVars :: Map Text (Map Text DictEntry)
    , mergedConfigs :: Map Text DictEntry
  }
  deriving Show

defaultContext :: AnalyzeContext
defaultContext = AnalyzeContext {
    globalVars = empty
    , defaultVars = empty
    , otherVars = empty
    , mergedConfigs = empty
  }


-- Mode used for the Hugo project (build, server).
data RunMode =
  BuildF
  | ServerF


data Template = Template {
    core :: HugoTemplCore
    , config :: Map Text DictEntry
    , layout :: LayoutTmpl
  }
  deriving Show

type ThemeTmplPages = Map Text PageTmpl
type ThemeTmplMaps = Map Text ThemeTmplPages
data LayoutTmpl = LayoutTmpl {
    topLevel :: ThemeTmplPages
    , defaults :: ThemeTmplPages
    , kinds :: Map Text ThemeTmplPages
    , partials :: Map Text ThemeTmplPages
    , shortcodes :: ThemeTmplPages
  }
  deriving Show

data PageKind =
  Home
  | Section
  | Taxonomy
  | Term
  | Single
  | List
  | Summary
  | Other Text
  deriving Show

data PageTmpl =
  FileRef FileWithPath
  | Compiled CompiledTemplate
  deriving Show

type CompiledTemplate = ByteString


data HugoComponents = HugoComponents {
    templCore :: HugoTemplCore
    , config :: [ FileWithPath ]
    , content :: [ FileWithPath ]
    , public :: [ FileWithPath ]
    , themes :: ThemeMap
    , staticDest :: FilePath
  }
  deriving Show


data HugoTemplCore = HugoTemplCore {
    archetypes :: [ FileWithPath ]
    , assets :: [ FileWithPath ]
    , dataSet :: [ FileWithPath ]
    , i18n :: [ FileWithPath ]
    , layouts :: [ FileWithPath ]
    , resource :: [ FileWithPath ]
    , static :: [ FileWithPath ]
    , projConfig :: [ FileWithPath ]
    , miscs :: [ FileWithPath ]
  }
  deriving Show

type ThemeMap = Map String HugoTemplCore

