module Main where

import qualified Control.Exception as Cexc
import qualified System.Environment as Senv
import qualified System.IO.Error as Serr

import qualified Options as Opt
import qualified MainLogic as Ml
import qualified Conclusion as Cnc

import GHC.IO.Handle (hSetEncoding)
import GHC.IO.StdHandles (stdout)
import GHC.IO.Encoding (utf8)


main :: IO ()
main = do
  hSetEncoding stdout utf8
  eiOptions <- Opt.parseCliOptions
  case eiOptions of
    Left errMsg ->
      putStrLn $ "err: " <> errMsg
    Right cliOptions -> do
      mbFileOptions <- 
        case cliOptions.configFile of
          Nothing -> do
            eiEnvConfFile <- Cexc.try $ Senv.getEnv "DANIELLCONF" :: IO (Either Serr.IOError String)
            case eiEnvConfFile of
              Left errMsg -> do
                confPath <- Opt.defaultConfigFilePath
                Opt.parseFileOptions confPath
              Right aPath -> Opt.parseFileOptions aPath
          Just aPath -> Opt.parseFileOptions aPath
      case mbFileOptions of
        Left errMsg -> putStrLn $ "err: " <> errMsg
        Right fileOptions ->
          Ml.runWithOptions cliOptions fileOptions >>= Cnc.conclude


