module ProjectDefinition.NextJS where

import qualified FileSystem.Types as Fs
import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))
import Generator.Types (WorkPlan (..))
import ProjectDefinition.Types



defaultNextJS :: NextJSComponents
defaultNextJS = NextJSComponents {
    config = NextJSConfig {
        envConfig = []
        , nextConfig = []
        , packageConfig = []
        , tsConfig = []
      }
    , components = []
    , pages = []
    , api = []
    , lib = []
    , styles = []
    , utils = []
    , hooks = []
    , services = []
    , types = []
    , tests = []
    , stories = []
    , public = []
    , build = []
    , deploy = []
  }


analyseNextJsProject :: RunOptions -> Bool -> Fs.PathFiles -> Either GenError WorkPlan
analyseNextJsProject rtOpts isStatic pathFiles =
  {- TODO:
    -- go through all the pathFile and build a ProjectDefinition.
      -> ProjectDefinition rtOpts.baseDir (WebApp (NextJS defaultNextJS)) [] pathFiles
    - pass to buildPlan
     - buildPlan: RunOptions -> ProjectDefinition -> IO (Either GenError WorkPlan)

  -}
  let
    content = classifyContent rtOpts pathFiles
  in
  if isStatic then
    analyseStaticProject rtOpts content
  else
    analyseWebAppProject rtOpts content


classifyContent :: RunOptions -> Fs.PathFiles -> ProjectDefinition
classifyContent rtOpts pathFiles =
  let
    gaga = []
  in
  ProjectDefinition rtOpts.baseDir (WebApp (NextJS defaultNextJS)) [] pathFiles


analyseStaticProject :: RunOptions -> ProjectDefinition -> Either GenError WorkPlan
analyseStaticProject rtOpts pathFiles =
  Right $ WorkPlan { destDir = "", items = [] }


analyseWebAppProject :: RunOptions -> ProjectDefinition -> Either GenError WorkPlan
analyseWebAppProject rtOpts pathFiles =
  Right $ WorkPlan { destDir = "", items = [] }
