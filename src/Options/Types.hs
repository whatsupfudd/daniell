module Options.Types where

import Data.Text (Text)
import Data.Vector.Generic.New (New)


data ProjectKind =
  SitePK
  | WebAppPK
  | LocalAppPK
  deriving Show


data NewOptions = NewOptions {
    projKind :: ProjectKind
    , rootDir :: FilePath
    , templates :: [ FilePath ]
    , compatMode :: Maybe Text
    , params :: [ ParameterTpl ]
  }
  deriving Show


data AnalyseOptions = AnalyseOptions {
    techKind :: TechKind
    , projDir :: Maybe FilePath
  }
  deriving Show


data TechKind =
  HaskellTK
  | PhpTK
  | TrytonTK
  | DjangoTK
  | PythonTK
  | CppTK
  deriving Show


data ParameterTpl =
  AssignmentP (Text, Text)
  | FlagP Text
  deriving Show


data BuildOptions = BuildOptions {
    kind :: BuildKind
    , srcDir :: Maybe Text
  }
  deriving Show

newtype BundleOptions = BundleOptions {
    destination :: FilePath
  }
  deriving Show

data BuildKind =
  SiteBK SiteOptions
  | WebAppBK WebAppOptions
  | LocalAppBK
  deriving Show

data SiteOptions =
  HugoSS HugoBuildOptions
  | NextSS
  | FuddleSS
  | GatsbySS
  | PhpSS TopDirOptions
  deriving Show


data WebAppOptions =
  NextWA NextJSBuildOptions
  | EwWappWA
  deriving Show


data HugoBuildOptions = HugoBuildOptions {
    baseURL :: Maybe Text
    , buildDrafts :: Maybe Bool
    , buildExpired :: Maybe Bool
    , buildFuture :: Maybe Bool
    , cacheDir :: Maybe Text
    , cleanDestinationDir :: Maybe Bool
    , clock :: Maybe Text
    , configFiles :: Maybe Text
    , configDir :: Maybe Text
    , contentDir :: Maybe Text
    , debug :: Maybe Bool
    , destination :: Maybe Text
    , disableKinds :: Maybe Text
    , enableGitInfo :: Maybe Bool
    , environment :: Maybe Text
    , forceSyncStatic :: Maybe Bool
    , gc :: Maybe Bool
    , ignoreCache :: Maybe Bool
    , ignoreVendorPaths :: Maybe Text
    , layoutDir :: Maybe Text
    , logLevel :: Maybe Text
    , minify :: Maybe Bool
    , noBuildLock :: Maybe Bool
    , noChmod :: Maybe Bool
    , noTimes :: Maybe Bool
    , panicOnWarning :: Maybe Bool
    , poll :: Maybe Text
    , printI18nWarnings :: Maybe Bool
    , printMemoryUsage :: Maybe Bool
    , printPathWarnings :: Maybe Bool
    , printUnusedTemplates :: Maybe Bool
    , quiet :: Maybe Bool
    , renderToMemory :: Maybe Bool
    , source :: Maybe Text
    , templateMetrics :: Maybe Bool
    , templateMetricsHints :: Maybe Bool
    , theme :: Maybe Text
    , themesDir :: Maybe Text
    , trace :: Maybe Text
    , verbose :: Maybe Bool
    , watch :: Maybe Bool
  }
  deriving Show


newtype TopDirOptions = TopDirOptions {
    srcDir :: Maybe Text
  }
  deriving Show


newtype NextJSBuildOptions = NextJSBuildOptions {
    packagePath :: Maybe Text
  }
  deriving Show
