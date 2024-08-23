module ProjectDefinition.Logic where

import Control.Monad (unless)

import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..))
import qualified Conclusion as Ccl
import Template.Types (ProjectTempl (..))
import Template.Project (mergeTemplates)
import Generator.Logic (buildWorkPlan, runGen)
import qualified System.Directory as SE

createProject :: RunOptions -> NewOptions -> [ProjectTempl] -> IO (Either Ccl.GenError ())
createProject rtOpts newOpts templates = do
  putStrLn "@[createProject] starting."
  -- move template(s) info into a work plan.
  let
    mergedTemplate = mergeTemplates templates
    workPlan = buildWorkPlan rtOpts newOpts mergedTemplate
  -- pass the work plan to the generator.
  destDirExist <- SE.doesDirectoryExist newOpts.rootDir
  unless destDirExist $ SE.createDirectory newOpts.rootDir
  rezA <- runGen rtOpts mergedTemplate workPlan
  pure $ Right ()
