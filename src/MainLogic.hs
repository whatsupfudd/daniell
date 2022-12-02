module MainLogic
where

import qualified Data.Text as DT
import qualified System.Environment as Env

import qualified Options as Opt
import qualified Commands as Cmd
import qualified Conclusion as Cnc

-- TODO: transfer to the Options package:
data EnvOptions = EnvOptions {
    danHome :: Maybe DT.Text
  }

runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO Cnc.Conclusion
runWithOptions cliOptions fileOptions =
  case cliOptions.job of
    Nothing -> do
      putStrLn "@[runWithOptions] start on nil command."
      pure Cnc.NilCcl
    Just aJob -> do
      -- Get environmental context in case it's required in the merge. Done here to keep the merge pure:
      mbDanHome <- Env.lookupEnv "DANIELLHOME"
      let
        envOptions = EnvOptions {
            danHome = DT.pack <$> mbDanHome
            -- TODO: put additional env vars.
          }
        rtOptions = mergeOptions cliOptions fileOptions envOptions 
        -- switchboard to command executors:
        cmdExecutor =
          case aJob of
            Opt.ConfigCmd -> Cmd.configHu
            Opt.ConvertCmd -> Cmd.convertHu
            Opt.DeployCmd -> Cmd.deployHu
            Opt.EnvCmd -> Cmd.envHu
            Opt.GenCmd -> Cmd.genHu
            Opt.HelpCmd -> Cmd.helpHu
            Opt.ImportCmd -> Cmd.importHu
            Opt.ListCmd -> Cmd.listHu
            Opt.ModCmd -> Cmd.modHu
            Opt.NewCmd -> Cmd.newHu
            Opt.ServerCmd -> Cmd.serverHu
            Opt.VersionCmd -> Cmd.versionHu
            -- Daniell specific:
            Opt.PublishCmd -> Cmd.publishDan
      result <- cmdExecutor rtOptions
      -- TODO: return a properly kind of conclusion.
      pure result

-- | mergeOptions gives priority to CLI options, followed by config-file options, followed
--   by environment variables.
mergeOptions :: Opt.CliOptions -> Opt.FileOptions -> EnvOptions -> Opt.RunOptions
mergeOptions cli file env =
  -- TODO: put proper priority filling of values for the Runtime Options.
  Opt.defaultRun "test"