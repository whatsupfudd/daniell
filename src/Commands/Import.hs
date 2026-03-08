module Commands.Import where

import Control.Monad.Cont (runContT)

import System.Directory (doesDirectoryExist)
import Hasql.Pool (Pool)

import qualified DB.Connect as Db
import qualified Conclusion as Ccl
import qualified Options.Runtime as Rto
import qualified Options.Types as Opts

import qualified ProjectDefinition.Php.Analyse as Php
import qualified ProjectDefinition.Html.Analyse as Html


importCmd :: Opts.ImportOptions -> Rto.RunOptions -> IO Ccl.Conclusion
importCmd importOpts rtOpts = do
  dirStatus <- doesDirectoryExist importOpts.sourceDir
  if dirStatus && knowProjType importOpts.specifics then
    let
      dbPool = Db.startPg rtOpts.pgDbConf
    in
    runContT dbPool (importProject importOpts rtOpts)
  else do
    if dirStatus then
      putStrLn $ "@[importCmd] project type is not supported: " <> show importOpts.sourceDir
    else
      putStrLn $ "@[importCmd] directory does not exist: " <> show importOpts.sourceDir
    pure Ccl.NilCcl


knowProjType :: Opts.ImportSpecifics -> Bool
knowProjType specifics =
  case specifics of
    Opts.PhpIS -> True
    Opts.DjangoIS -> True
    Opts.RailsIS -> True
    Opts.TrytonIS -> True
    Opts.HtmlIS -> True
    _ -> False


importProject :: Opts.ImportOptions -> Rto.RunOptions -> Pool -> IO Ccl.Conclusion
importProject importOpts rtOpts dbPool = do
  case importOpts.specifics of
    Opts.PhpIS -> do
      importPhp importOpts rtOpts dbPool
    Opts.DjangoIS -> do
      putStrLn "@[importCmd] DjangoIS" >> pure Ccl.NilCcl
    Opts.RailsIS -> do
      putStrLn "@[importCmd] RailsIS" >> pure Ccl.NilCcl
    Opts.TrytonIS -> do
      putStrLn "@[importCmd] TrytonIS" >> pure Ccl.NilCcl
    Opts.HtmlIS -> do
      importHtml importOpts rtOpts dbPool
    _ -> do
      putStrLn "@[importCmd] Unknown specifics"
      pure Ccl.NilCcl


importHtml :: Opts.ImportOptions -> Rto.RunOptions -> Pool -> IO Ccl.Conclusion
importHtml importOpts rtOpts dbPool = do
  putStrLn $ "@[importHtml] starting, dir: " <> show importOpts.sourceDir <> ", name: " <> show importOpts.projectName
  Html.processDir importOpts.projectName importOpts.sourceDir dbPool
  pure Ccl.NilCcl


importPhp :: Opts.ImportOptions -> Rto.RunOptions -> Pool -> IO Ccl.Conclusion
importPhp importOpts rtOpts dbPool = do
  putStrLn $ "@[importPhp] starting, dir: " <> show importOpts.sourceDir <> ", name: " <> show importOpts.projectName
  Php.processDir importOpts.projectName importOpts.sourceDir dbPool
  pure Ccl.NilCcl