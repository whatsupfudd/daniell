module ProjectDefinition.Hugo.Types where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text)

import Options.Types (HugoBuildOptions (..))
import qualified FileSystem.Types as Fs
import Generator.Types (ExecSystem (..), WorkPlan (..))
import ProjectDefinition.Types (DictEntry)

import qualified Cannelle.Template.Types as Ct

-- Analysis context for a Hugo project (configurations, etc) that will be used to drive generation.
data AnalyzeContext = AnalyzeContext {
    globalVars :: Mp.Map Text DictEntry
    , defaultVars :: Mp.Map Text DictEntry
    , otherVars :: Mp.Map Text (Mp.Map Text DictEntry)
    , mergedConfigs :: Mp.Map Text DictEntry
  }
  deriving Show

defaultContext :: AnalyzeContext
defaultContext = AnalyzeContext {
    globalVars = Mp.empty
    , defaultVars = Mp.empty
    , otherVars = Mp.empty
    , mergedConfigs = Mp.empty
  }


-- Mode used for the Hugo project (build, server).
data RunMode =
  BuildF
  | ServerF


data Template = Template {
    core :: HugoTemplCore
    , config :: Mp.Map Text DictEntry
    , layout :: LayoutTmpl
  }
  deriving Show

type ThemeTmplPages = Mp.Map Text PageTmpl
type ThemeTmplMaps = Mp.Map Text ThemeTmplPages
data LayoutTmpl = LayoutTmpl {
    topLevel :: ThemeTmplPages
    , defaults :: ThemeTmplPages
    , kinds :: Mp.Map Text ThemeTmplPages
    , partials :: Mp.Map Text ThemeTmplPages
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
  FileRef Fs.FileWithPath
  | Compiled CompiledTemplate
  deriving Show

type CompiledTemplate = ByteString


data HugoComponents = HugoComponents {
    templCore :: HugoTemplCore
    , config :: [ Fs.FileWithPath ]
    , content :: [ Fs.FileWithPath ]
    , public :: [ Fs.FileWithPath ]
    , themes :: ThemeMap
    , staticDest :: FilePath
  }
  deriving Show


data HugoTemplCore = HugoTemplCore {
    archetypes :: [ Fs.FileWithPath ]
    , assets :: [ Fs.FileWithPath ]
    , dataSet :: [ Fs.FileWithPath ]
    , i18n :: [ Fs.FileWithPath ]
    , layouts :: [ Fs.FileWithPath ]
    , resource :: [ Fs.FileWithPath ]
    , static :: [ Fs.FileWithPath ]
    , projConfig :: [ Fs.FileWithPath ]
    , miscs :: [ Fs.FileWithPath ]
  }
  deriving Show

type ThemeMap = Mp.Map String HugoTemplCore

-- WorkPlan relate data types:

data CTPair = CTPair {
    kind :: Fs.FileKind
    , contentPrefix :: Int32
    , section :: FilePath
    , content :: Fs.FileItem
    , themePrefix :: Int32
    , template :: FilePath
  }
  deriving Show


type HgWorkPlan = WorkPlan HgEngine HgContext HgWorkItem

newtype HgEngine = HgEngine { hugoOpts :: HugoBuildOptions }
  deriving Show

data HgContext = HgContext {
      mergedConfigs :: Mp.Map Text DictEntry
    , pathPrefixes :: Mp.Map Int32 FilePath
    , compTemplates :: Mp.Map FilePath Ct.TemplateDef
  }
  deriving Show

data HgWorkItem =
  ExecTmplForContent CTPair
  | ExecTmplForRoute { dirPath :: FilePath }

