module Template.Parser where


import Data.Text (Text)

import Options.Runtime (RunOptions (..))
import ProjectDefinition.Types (ProjectDefinition (..))
import Template.Types (Template (..))


-- TODO: the main entry point for parsing a template file (source code for creating content).
parse :: RunOptions -> ProjectDefinition -> FilePath -> IO (Either Text Template)
parse rtOpts siteDef tmplName =
  pure $ Left "@[parse] TODO"
