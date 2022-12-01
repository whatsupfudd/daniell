module MainLogic
where

import qualified Options as Opt

runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO ()
runWithOptions cliOptions fileOptions =
  putStrLn "@[runWithOptions] start."
