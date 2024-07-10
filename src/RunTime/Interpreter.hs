module RunTime.Interpreter where

import Data.Text (Text)

import Conclusion (GenError (..))
import Options.Runtime (RunOptions (..))
import ProjectDefinition.Types (ProjectDefinition (..), TmpFileDef (..))
import Markup.Types (MarkupPage)
import Template.Types (Template)

-- TODO:
type ExecContext = Bool


execute :: RunOptions -> ProjectDefinition -> Template -> ExecContext -> MarkupPage -> IO (Either GenError ExecContext)
execute rtOpts siteDef template execCtxt contentGen =
  pure . Left $ SimpleMsg "@[execute] TODO"
  -- run the interpreter on the code in template for contentGen page, within execCtxt.


createContext :: RunOptions -> ExecContext
-- TODO:
createContext rtOpts = True