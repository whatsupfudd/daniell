{-# LANGUAGE MultiParamTypeClasses #-}
module ProjectDefinition.Hugo.Work (runWorkItem) where

import Control.Monad (foldM)
import Control.Exception (try)

import qualified Data.ByteString as Bs
import qualified Data.Map as Mp
import Data.Text (pack)

import System.FilePath ((</>))

import Options.Runtime (RunOptions (..), TechOptions (..))
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import Generator.Types (ExecSystem (..), WorkPlan (..))
import Template.Hugo (compileTemplate)

import qualified Cannelle.FileUnit.InOut as Fio

import Cannelle.FileUnit.Types (FileUnit (..), ImportTpl (..))
import ProjectDefinition.Hugo.Types


instance Show HgWorkItem where
  show wi = case wi of
    ExecTmplForContent ctp -> "ExecTmplForContent > dirPath: " <> case ctp.section of "" -> "<nil>"; aPath -> aPath <> ", content" <> show ctp.content <> ", template: " <> show ctp.templates
    ExecTmplForRoute aDirPath -> "ExecTmplForRoute > dirPath: " <> aDirPath


instance ExecSystem HgEngine HgContext HgWorkItem where
  runWorkItem rtOpts engine context wItem = do
    case wItem of
      ExecTmplForRoute dirPath -> do
        putStrLn $ "@[Hugo.runWorkItem] src: " <> show wItem
        pure $ Right context
      ExecTmplForContent ctp ->
        let
          mbContentPath = Mp.lookup ctp.contentPrefix context.pathPrefixes
          mbThemePath = Mp.lookup ctp.themePrefix context.pathPrefixes
          mkPath = case mbContentPath of
            Nothing -> ctp.section </> Fs.getItemPath ctp.content
            Just aPath -> aPath </> ctp.section </> Fs.getItemPath ctp.content
          rebasedTmplPaths = map (\aTmplPath -> case mbThemePath of
                Nothing -> rtOpts.baseDir </> "layouts" </> aTmplPath
                Just aPath -> aPath </> aTmplPath
              ) ctp.templates
        in do
        -- putStrLn $ "@[Hugo.runWorkItem] content: " <> mkPath
        putStrLn $ "@[Hugo.runWorkItem] content: <content>/"
            <> (case ctp.section of "" -> ""; aPath -> aPath <> "/")
            <> show (case ctp.content of Fs.KnownFile _ aPath -> aPath; _ -> "<?>")
            <> " using " <> show ctp.templates <> "."
        tmplCtxt <- foldM (\accCtxt aTmplPath -> do
          -- First, get the template(s) (either already compiled, or compile and add to the context).
          case accCtxt of
            Left err -> pure $ Left err
            Right aContext -> do
              case Mp.lookup aTmplPath aContext.compFileUnits of
                Nothing -> do
                  eiTemplSource <- try $ Bs.readFile aTmplPath :: IO (Either IOError Bs.ByteString)
                  case eiTemplSource of
                    Left err ->
                      let
                        errMsg =  "@[Hugo.runWorkItem] readFile err: " <> show err
                      in do
                      putStrLn errMsg
                      pure . Left . SimpleMsg $ pack errMsg
                    Right templSource -> do
                      compileTemplate rtOpts (engine, aContext, wItem) (aTmplPath, templSource)
                Just fileUnit -> do
                  -- putStrLn $ "@[Hugo.runWorkItem] template already parsed: " <> aTmplPath
                  pure $ Right context
          ) (Right context) rebasedTmplPaths
        -- Second, do the rest.
        case tmplCtxt of
          Left err -> pure $ Left err
          Right aContext -> do
            -- TODO:
            -- - prepare the runtime context with the info about the source doc,
            -- - merge all FU into a single module.
            -- - VM.exec the module with right context.
            -- - save the output file.
            resolveImports aContext rebasedTmplPaths mbThemePath (rtOpts.baseDir </> "layouts")


resolveImports :: HgContext -> [FilePath] -> Maybe FilePath -> FilePath -> IO (Either GenError HgContext)
resolveImports aContext tmplPaths mbThemePath layoutDir = do
  foldM (\accum aTmplPath ->
    case Mp.lookup aTmplPath aContext.compFileUnits of
      Nothing -> do
        putStrLn $ "@[Hugo.runWorkItem] file unit: " <> aTmplPath <> ", not found."
        pure accum
      Just aFileUnit -> do
        -- putStrLn $ "@[Hugo.runWorkItem] file unit: " <> aTmplPath <> ", imports: " <> show aFileUnit.imports
        mapM_ (\anImport ->
            putStrLn $ "@[Hugo.runWorkItem] import: " <> Fio.showImportTpl aFileUnit anImport
          ) aFileUnit.imports
        pure accum
    ) (Right aContext) tmplPaths
        

