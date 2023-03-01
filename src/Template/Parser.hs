module Template.Parser where


import Data.Text (Text)

import Options.RunOptions (RunOptions (..))
import SiteDefinition.Types (SiteDefinition (..))
import Template.Types (Template (..))


-- TODO:
parse :: RunOptions -> SiteDefinition -> FilePath -> IO (Either Text Template)
parse rtOpts siteDef tmplName =
  pure $ Left "@[parse] TODO"
