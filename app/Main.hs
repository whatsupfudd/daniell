module Main where

import qualified Options as Opt
import qualified MainLogic as Ml

main :: IO ()
main = do
  eiOptions <- Opt.parseCliOptions
  case eiOptions of
    Left errMsg ->
      putStrLn $ "err: " <> errMsg
    Right cliOptions -> do
      mbFileOptions <- 
        case cliOptions.configFile of
          Nothing -> Opt.parseFileOptions Opt.defaultConfigFilePath
          Just aPath -> Opt.parseFileOptions aPath
      case mbFileOptions of
        Left errMsg -> putStrLn $ "err: " <> errMsg
        Right fileOptions -> Ml.runWithOptions cliOptions fileOptions


