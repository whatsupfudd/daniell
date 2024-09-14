{-# LANGUAGE MultiParamTypeClasses #-}

module ProjectDefinition.Fuddle where


import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import Generator.Types (ExecSystem (..), WorkPlan (..))

type FdWorkPlan = WorkPlan FdEngine FdContext FdWorkItem
data FdEngine = FdEngine
  deriving Show
data FdContext = FdContext
  deriving Show
data FdWorkItem = FdWorkItem
  deriving Show

instance ExecSystem FdEngine FdContext FdWorkItem where
  runWorkItem _ _ _ _= pure $ Right FdContext

analyseFuddleProject :: RunOptions -> Bool -> Fs.PathFiles -> IO (Either GenError FdWorkPlan)
analyseFuddleProject rtOpts isStatic pathFiles =
  -- TODO:
  pure . Left $ SimpleMsg "Fuddle project not implemented yet."
