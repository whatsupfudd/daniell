module ProjectDefinition.Gatsby where

import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import Generator.Types (WorkPlan, Engine, Context, WorkItem)

type GbWorkPlan = WorkPlan GbEngine GbContext GbWorkItem
data GbEngine = GbEngine
instance Show GbEngine where
  show _ = "@[gbEngine] "
instance Engine GbEngine

data GbContext = GbContext
instance Context GbContext
instance Show GbContext where
  show _ = "@[gbContext] "

data GbWorkItem = GbWorkItem
instance WorkItem GbWorkItem
instance Show GbWorkItem where
  show _ = "@[gbWorkItem] "

analyseGatsbyProject :: RunOptions -> Fs.PathFiles -> IO (Either GenError GbWorkPlan)
analyseGatsbyProject rtOpts pathFiles =
  -- TODO:
  pure . Left $ SimpleMsg "Gatsby project not implemented yet."
