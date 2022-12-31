module RunTime.StaticSite where

import qualified Options.RunOptions as Rto
import qualified Options.Config as Cfgp
import qualified SiteDefinition.Explore as Expl
import qualified Markup.Markdown as Mrkp


data StaticSite = 
  InMemory
  | OnStorage

