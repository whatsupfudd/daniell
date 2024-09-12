module ProjectDefinition.AssocRules where

import ProjectDefinition.Types (ProjectDefinition (..))
import Markup.Types (MarkupPage (..))

{-
 TODO:
 * Provide logic to match all templates for each MarkupPage.
 * Provide the runtime engine required for handling the logic of the templates (WorkPlan type).
 * Porivde the output generator required to create the proper format of content (Transformation type).
-}

findTemplForContent :: a -> MarkupPage -> FilePath
findTemplForContent siteDef contentGen =
  -- TODO: use page kind, page section, other rules
  "/tmp/gaga"