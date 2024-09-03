module ProjectDefinition.Hugo where

import qualified FileSystem.Types as Fs
import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))
import Generator.Types (WorkPlan (..))

import ProjectDefinition.Types

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


analyseHugoProject :: RunOptions -> Fs.PathFiles -> Either GenError WorkPlan
analyseHugoProject rtOpts pathFiles =
  -- TODO:
  -- Right $ ProjectDefinition rtOpts.baseDir (Site (Hugo defaultComponents)) [] pathFiles
  Right $ WorkPlan { destDir = "", items = []}

