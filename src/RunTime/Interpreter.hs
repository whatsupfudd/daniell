module RunTime.Interpreter where

import Data.Text (Text)

import Conclusion (GenError (..))
import Options.RunOptions (RunOptions (..))
import SiteDefinition.Types (SiteDefinition (..))
import Markup.Types (MarkupPage)
import Template.Types (Template)

-- TODO:
type ExecContext = Bool


execute :: RunOptions -> SiteDefinition -> Template -> ExecContext -> MarkupPage -> IO (Either GenError ExecContext)
execute rtOpts siteDef template execCtxt contentGen =
  pure . Left $ SimpleMsg "@[execute] TODO"
  -- run the interpreter on the code in template for contentGen page, within execCtxt.


createContext :: RunOptions -> ExecContext
-- TODO:
createContext rtOpts = True