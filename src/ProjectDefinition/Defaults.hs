module ProjectDefinition.Defaults where

import Options.Runtime (RunOptions (..))
-- import ProjectDefinition.Types (ProjectType (..), SiteType (..), WebAppType (..), LocalAppType (..))
import Options.Types (ProjectKind (..))

-- TODO: Use the RunOptions to determine the sub-kind of project to create from the NewProjectKind one.
defaultLocations :: RunOptions -> ProjectKind -> FilePath
defaultLocations rtOpts pType =
  let
    templateName = case pType of
      SitePK -> "hugo"
      {-
      Site (Hugo _) -> "hugo"
      Site (WordPress _) -> "wordpress"
      -}
      WebAppPK -> "nextjs"
      {-
      WebApp (NextJS _) -> "nextjs"
      WebApp (Fuddle _) -> "fuddle"
      -}
      LocalAppPK -> "fuddapp"
      {-
      LocalApp (FuddApp _) -> "fuddapp"
      -}
  in
    rtOpts.templateDir <> "/" <> templateName <> ".dtmpl"


