module ProjectDefinition.Logic where

import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..))
import qualified Conclusion as Ccl
import Template.Types (ProjectTempl (..))
import Template.Project (mergeTemplates)
import Generator.Logic (buildWorkPlan, runGen)

createProject :: RunOptions -> NewOptions -> [ProjectTempl] -> IO (Either Ccl.GenError ())
createProject rtOpts newOpts templates = do
  -- move template(s) info into a work plan.
  let
    mergedTemplate = mergeTemplates templates
    workPlan = buildWorkPlan rtOpts newOpts mergedTemplate
  -- pass the work plan to the generator.
  rezA <- runGen rtOpts workPlan
  putStrLn "@[createProject] starting."
  pure $ Right ()
