{-# LANGUAGE MultiParamTypeClasses #-}

module ProjectDefinition.Gatsby where

import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import Generator.Types (ExecSystem (..), WorkPlan (..))

type GbWorkPlan = WorkPlan GbEngine GbContext GbWorkItem
data GbEngine = GbEngine
  deriving Show
data GbContext = GbContext
  deriving Show
data GbWorkItem = GbWorkItem
  deriving Show

instance ExecSystem GbEngine GbContext GbWorkItem where
  runWorkItem _ _ _ _ = pure $ Right GbContext

analyseGatsbyProject :: RunOptions -> Fs.PathFiles -> IO (Either GenError GbWorkPlan)
analyseGatsbyProject rtOpts pathFiles =
  -- TODO:
  pure . Left $ SimpleMsg "Gatsby project not implemented yet."
