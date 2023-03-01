module Commands  (
  module Commands.Config
  , module Commands.Convert
  , module Commands.Deploy
  , module Commands.Env
  , module Commands.Gen
  , module Commands.Help
  , module Commands.Import
  , module Commands.List
  , module Commands.Mod
  , module Commands.New
  , module Commands.Server
  , module Commands.Version
  , module Commands.Publish
  , mergeOptions
 )
where

import Commands.Config
import Commands.Convert
import Commands.Deploy
import Commands.Env
import Commands.Gen
import Commands.Help
import Commands.Import
import Commands.List
import Commands.Mod
import Commands.New
import Commands.Server
import Commands.Version
import Commands.Publish

import qualified Options as Opt

-- | mergeOptions gives priority to CLI options, followed by config-file options, followed
--   by environment variables.
mergeOptions :: Opt.CliOptions -> Opt.FileOptions -> Opt.EnvOptions -> Opt.RunOptions
mergeOptions cli file env =
  -- TODO: put proper priority filling of values for the Runtime Options.
  Opt.defaultRun "test"