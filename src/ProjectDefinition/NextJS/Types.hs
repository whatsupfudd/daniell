module ProjectDefinition.NextJS.Types where

import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text)


import Options.Types (NextJSBuildOptions (..))
import Generator.Types (ExecSystem (..), WorkPlan (..))
import ProjectDefinition.Types (DictEntry)
import FileSystem.Types (FileWithPath, FileItem)
import qualified Cannelle.FileUnit.Types as Fu

{- NextJS -}

data NextJSComponents = NextJSComponents {
      config :: NextJSConfig
      , components :: [ FileWithPath ]
      , pages :: [ FileWithPath ]
      , api :: [ FileWithPath ]
      , lib :: [ FileWithPath ]
      , styles :: [ FileWithPath ]
      , utils :: [ FileWithPath ]
      , hooks :: [ FileWithPath ]
      , services :: [ FileWithPath ]
      , types :: [ FileWithPath ]
      , tests :: [ FileWithPath ]
      , stories :: [ FileWithPath ]
      , public :: [ FileWithPath ]
      , build :: [ FileWithPath ]
      , deploy :: [ FileWithPath ]
      , miscs :: [ FileWithPath ]
    }
    deriving Show

data NextJSConfig = NextJSConfig {
    envConfig :: [ FileItem ]
    , nextConfig :: [ FileItem ]
    , packageConfig :: [ FileItem ]
    , tsConfig :: [ FileItem ]
  }
  deriving Show


type NxWorkPlan = WorkPlan NxEngine NxContext NxWorkItem

newtype NxEngine = NxEngine { nextjsOpts :: NextJSBuildOptions }
  deriving Show

data NxContext = NxContext {
      mergedConfigs :: Mp.Map Text DictEntry
    , pathPrefixes :: Mp.Map Int32 FilePath
    , compFileUnits :: Mp.Map FilePath Fu.FileUnit
    , siteDef :: NextJSComponents
  }
  deriving Show

data NxWorkItem =
  ExecTmplForContentNx { dirPath :: FilePath }
  | ExecTmplForRouteNx { dirPath :: FilePath }

instance Show NxWorkItem where
  show wi = case wi of
    ExecTmplForContentNx aDirPath -> "ExecTmplForContentNx > dirPath: " <> aDirPath
    ExecTmplForRouteNx aDirPath -> "ExecTmplForRouteNx > dirPath: " <> aDirPath
