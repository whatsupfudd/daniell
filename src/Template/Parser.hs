module Template.Parser where


import Data.Text (Text)

import Options.Runtime (RunOptions (..))
import SiteDefinition.Types (SiteDefinition (..), TmpFileDef (..))
import Template.Types (Template (..))


-- TODO:
parse :: RunOptions -> SiteDefinition TmpFileDef -> FilePath -> IO (Either Text Template)
parse rtOpts siteDef tmplName =
  pure $ Left "@[parse] TODO"
