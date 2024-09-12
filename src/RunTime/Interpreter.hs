module RunTime.Interpreter where

import Data.Text (Text)

import Conclusion (GenError (..))
import Options.Runtime (RunOptions (..))
import ProjectDefinition.Types (ProjectDefinition (..))
import Markup.Types (MarkupPage)
import Template.Types (Template)

-- TODO:
type ExecContext = Bool


{-
  The execution takes a template (can be the default template for the kind of markup/project...), 
  potentially a markup page (the data that feeds the template holes + logic values), and then runs the
  template to obtain the output which should be a the content of a text file (html or code).
-}

execute :: RunOptions -> ProjectDefinition -> Template -> ExecContext -> Maybe MarkupPage -> IO (Either GenError ExecContext)
execute rtOpts projDef template execCtxt contentGen =
  pure . Left $ SimpleMsg "@[execute] TODO"
  -- run the interpreter on the code in template for contentGen page, within execCtxt.


createContext :: RunOptions -> ExecContext
-- TODO:
createContext rtOpts = True