module ProjectDefinition.Fuddle where


import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import Generator.Types (WorkPlan, Engine, Context, WorkItem)

type FdWorkPlan = WorkPlan FdEngine FdContext FdWorkItem
data FdEngine = FdEngine
instance Show FdEngine where
  show _ = "@[fdEngine] "
instance Engine FdEngine

data FdContext = FdContext
instance Context FdContext
instance Show FdContext where
  show _ = "@[fdContext] "

data FdWorkItem = FdWorkItem
instance WorkItem FdWorkItem
instance Show FdWorkItem where
  show _ = "@[fdWorkItem] "


analyseFuddleProject :: RunOptions -> Bool -> Fs.PathFiles -> IO (Either GenError FdWorkPlan)
analyseFuddleProject rtOpts isStatic pathFiles =
  -- TODO:
  pure . Left $ SimpleMsg "Fuddle project not implemented yet."
