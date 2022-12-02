module MainLogic
where

import qualified Options as Opt
import qualified Commands as Cmd
import qualified Conclusion as Cnc


runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO Cnc.Conclusion
runWithOptions cliOptions fileOptions =
  case cliOptions.job of
    Nothing -> do
      putStrLn "@[runWithOptions] start on nil command."
      pure Cnc.NilCcl
    Just aJob -> do
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
      pure Cnc.NilCcl


mergeOptions :: Opt.CliOptions -> Opt.FileOptions -> Opt.RunOptions
mergeOptions cli file =
  Opt.defaultRun "test"