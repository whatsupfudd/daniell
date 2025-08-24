{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ProjectDefinition.Scaffold.Work where

-- TODO: change the module's name and a few details to set this as the Haskell scaffold specialized logic, as there is the Hugo and NextJS logic.


import Control.Monad (foldM, forM, forM_)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Foldable as Fld
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Mp
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified System.Directory as SE

import qualified Cannelle.VM.Context as Vm
import qualified Cannelle.VM.Engine as Vm
import qualified Cannelle.Hugo.Exec as He
import qualified Cannelle.Templog.Parse as Tp
import qualified Cannelle.Templog.Exec as Te
import qualified Cannelle.FileUnit.Types as Fu
import Cannelle.FileUnit.Types (FileUnit (..), FunctionDefTpl (..))


import Conclusion (GenError (..), Conclusion (..))
import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..), ProjectKind (..))
import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs
import ProjectDefinition.Types (ProjectDefinition (..), ProjectType (..), SiteType (..), WebAppType (..), LocalAppType (..))
import qualified ProjectDefinition.AssocRules as Rules
import qualified ProjectDefinition.Hugo as Hu
import qualified ProjectDefinition.NextJS as Nx
import ProjectDefinition.Defaults (defaultLocations)
import qualified Markup.Page as Mrkp
import Markup.Types (MarkupPage (..))

import Utils (splitResults)
import Generator.Types

import Scaffold.Types (ScaffoldBundle (..))

import ProjectDefinition.Scaffold.Types


type TemplateMatches = Mp.Map FilePath [ MarkupPage ]

-- *** DEPRECATED: should use the Fuddle or Haskell WorkEngine instead.
runGen :: RunOptions -> FilePath -> ScaffoldBundle -> NonEmpty ScfWorkItem -> IO (Either GenError ())
runGen rtOpts destDir projTempl wItems = do
  mapM_ (\wi -> do
      putStrLn $ "@[runGen] wi: " <> show wi
      runRez <- runItem rtOpts destDir projTempl wi
      case runRez of
        Left err -> do
          let
            errMsg = "@[runGen] runItem err: " <> show err
          putStrLn errMsg
          pure $ Left err
        Right _ -> pure $ Right ()
    ) wItems
  pure $ Right ()


instance ExecSystem ScfEngine ScfContext ScfWorkItem where
  runWorkItem rtOpts engine context item = do
    runItem rtOpts context.destDir context.bundle item
    pure $ Right context


runItem :: RunOptions -> FilePath -> ScaffoldBundle -> ScfWorkItem -> IO (Either GenError ())
runItem rtOpts destDir projTempl = \case
  NewDirIfNotExist dirPath -> do
    let fullDirPath = destDir <> "/" <> dirPath
    alreadyDir <- SE.doesDirectoryExist fullDirPath
    if alreadyDir then
      putStrLn $ "@[runItem] skipping existing NewDir: " <> fullDirPath
    else do
      putStrLn $ "@[runItem] making NewDir: " <> fullDirPath
      SE.createDirectory fullDirPath
    pure $ Right ()
  CloneSource srcPath destPath ->
    let
      fullDestPath = destDir <> "/" <> destPath
    in do
    putStrLn $ "@[runItem] CloneSource: " <> srcPath <> " -> " <> fullDestPath
    alreadyThere <- SE.doesFileExist fullDestPath
    if alreadyThere then
      putStrLn $ "@[runItem] skipping existing CloneSource: " <> fullDestPath
    else
      SE.copyFile srcPath fullDestPath
    pure $ Right ()
  CloneSourceImmediate content destPath ->
    let
      fullDestPath = destDir <> "/" <> destPath
    in do
    putStrLn $ "@[runItem] CloneSourceImmediate to: " <> fullDestPath
    Lbs.writeFile fullDestPath content
    pure $ Right ()
  DupFromSource fItem srcPath destPath ->
    let
      fullDestPath = destDir <> "/" <> destPath
    in do
    putStrLn $ "@[runItem] DupFromSource: " <> srcPath <> " -> " <> fullDestPath
    alreadyThere <- SE.doesFileExist fullDestPath
    if alreadyThere then
      putStrLn $ "@[runItem] skipping existing DupFromSource: " <> fullDestPath
    else
      SE.copyFile srcPath fullDestPath
    pure $ Right ()
  RunTemplate path -> do
    putStrLn $ "@[runItem] RunTemplate: " <> path
    pure $ Right ()
  RunTemplateImmediate content destPath ->
    let
      fullDestPath = destDir <> "/" <> destPath
    in do
    putStrLn $ "@[runItem] RunTemplateImmediate to: " <> fullDestPath
    Lbs.writeFile fullDestPath content
    pure $ Right ()
  RunTemplateToDest tKind dir tPath destPath -> do
    case tPath of
        Fs.KnownFile fType srcPath ->
          let
            fullPath = case projTempl.hasPrefix of
              Nothing -> projTempl.path <> "/" <> (if dir == "" then "" else dir <> "/") <> srcPath
              Just prefix -> projTempl.path <> "/" <> (if dir == "" then "" else dir <> "/") <> srcPath
          in
            genFileFromTemplate rtOpts tKind fullPath (destDir <> "/" <> destPath)
        Fs.MiscFile srcPath -> do
          putStrLn $ "@[runItem] RunTemplateToDest: " <> show tKind <> ", src: " <> show tPath <> ", dst: " <> destPath
          pure $ Right ()
  RunTemplateToDestImmediate tKind content destPath ->
    let
      fullDestPath = destDir <> "/" <> destPath
    in do
    putStrLn $ "@[runItem] RunTemplateToDestImmediate: " <> fullDestPath
    genFileFromTemplateImmediate rtOpts destPath tKind content fullDestPath

data ExecTemplate =
  CannelleVM Fu.FunctionDefTpl (V.Vector Vm.ConstantValue)
  | ConcatET BS.ByteString
  | Jinja String
  | EndExec
  deriving Show


genFileFromTemplate :: RunOptions -> Fs.FileKind -> FilePath -> FilePath -> IO (Either GenError ())
genFileFromTemplate rtOpts fType srcPath destPath = do
  putStrLn $ "@[genFileFromTemplate] starting: " <> srcPath <> " -> " <> destPath
  -- TODO: figure out a common interface amongst all template files for the VM execution.
  eiExecCmd <- case fType of
    Fs.Haskell -> do
      putStrLn $ "@[runItem] Haskell+Templog: " <> show srcPath
      -- read/ts-parse the template file
      rezA <- Tp.parse (rtOpts.debug /= 0) srcPath
      case rezA of
        Left err -> pure . Left . SimpleMsg . pack $ show err
        Right fileUnit ->
          -- TEST:
          if V.null fileUnit.definitions then
            pure $ Right EndExec
          else
            let
              anAction = V.head fileUnit.definitions
            in
            case anAction of
              Fu.CloneVerbatim _ -> do
                SE.copyFile srcPath destPath
                pure $ Right EndExec
              Fu.Concat crpMode content ->
                pure . Right $ ConcatET content
              Fu.Exec fctDef -> do
                pure . Right $ CannelleVM fctDef fileUnit.constants
              _ -> pure . Left . SimpleMsg . pack $ "@[genFileFromTemplate] logic " <> show anAction <> " not implemented."
    Fs.Markdown -> do
      putStrLn $ "@[genFileFromTemplate] no engine for: " <> show srcPath <> ", defaulting to CloneVerbatim."
      SE.copyFile srcPath destPath
      pure $ Right EndExec
    Fs.Yaml -> do
      putStrLn $ "@[genFileFromTemplate] no engine for: " <> show srcPath <> ", defaulting to CloneVerbatim."
      SE.copyFile srcPath destPath
      pure $ Right EndExec
  case eiExecCmd of
    Left err -> do
      putStrLn $ "@[genFileFromTemplate] eiFtemplate error: " <> show err
      pure $ Left err
    Right execCmd -> runExecCommand rtOpts fType execCmd destPath


genFileFromTemplateImmediate :: RunOptions -> FilePath ->Fs.FileKind -> Lbs.ByteString -> FilePath -> IO (Either GenError ())
genFileFromTemplateImmediate rtOpts srcPath fType content destPath = do
  putStrLn $ "@[genFileFromTemplateImmediate] starting: " <> destPath
  -- TODO: figure out a common interface amongst all template files for the VM execution.
  eiExecCmd <- case fType of
      Fs.Haskell -> do
        rezA <- Tp.parseFromContent (rtOpts.debug /= 0) srcPath (Lbs.toStrict content) Nothing
        case rezA of
          Left err -> pure . Left . SimpleMsg . pack $ show err
          Right fileUnit ->
            -- TEST:
            if V.null fileUnit.definitions then
              pure $ Right EndExec
            else
              let
                anAction = V.head fileUnit.definitions
              in
              case anAction of
                Fu.CloneVerbatim _ -> do
                  Lbs.writeFile destPath content
                  pure $ Right EndExec
                Fu.Concat crpMode content ->
                  pure . Right $ ConcatET content
                Fu.Exec fctDef -> do
                  pure . Right $ CannelleVM fctDef fileUnit.constants
                _ -> pure . Left . SimpleMsg . pack $ "@[genFileFromTemplate] logic " <> show anAction <> " not implemented."
      Fs.Markdown -> do
        putStrLn $ "@[genFileFromTemplate] no engine for: " <> show destPath <> ", defaulting to CloneVerbatim."
        Lbs.writeFile destPath content
        pure $ Right EndExec
      Fs.Yaml -> do
        putStrLn $ "@[genFileFromTemplate] no engine for: " <> show destPath <> ", defaulting to CloneVerbatim."
        Lbs.writeFile destPath content
        pure $ Right EndExec
      _ -> do
        putStrLn $ "@[genFileFromTemplate] no engine for: " <> show destPath <> ", defaulting to CloneVerbatim."
        Lbs.writeFile destPath content
        pure $ Right EndExec
  case eiExecCmd of
    Left err -> do
      putStrLn $ "@[genFileFromTemplate] eiFtemplate error: " <> show err
      pure $ Left err
    Right execCmd -> runExecCommand rtOpts fType execCmd destPath


runExecCommand :: RunOptions -> Fs.FileKind -> ExecTemplate -> FilePath -> IO (Either GenError ())
runExecCommand rtOpts fType execCmd destPath = do
  case execCmd of
    CannelleVM fctDef constants ->
      let
        fctTest = Vm.FunctionDef {
                moduleID = 0
                , fname = fctDef.name
                , args = Nothing
                , returnType = Vm.FirstOrderSO Vm.IntTO
                , heapSize = 32
                , body = Vm.ByteCode fctDef.bytecode
              }
        vmModule = Vm.VMModule {
          functions = V.singleton fctTest
        , fctMap = Mp.singleton fctDef.name 0
        , constants = constants
        , externModules = Mp.empty
      }
      in do
      putStrLn $ "@[genFileFromTemplate] Haskell templ, executing VM module: " <> show vmModule
      -- TODO: create a runtime context, execute the VM on the FileTemplate produced:
      -- TODO: pass the proper context, HugoLib context isn't appropriate for all cases.
      -- Hg.fakeHugoContext
      eiRez <- Vm.execModule vmModule Te.fakeTemplogContext Nothing  
      case eiRez of
        Left errMsg -> do
          putStrLn $ "@[genFileFromTemplate] VM error: " <> show errMsg
          pure $ Left $ SimpleMsg (pack errMsg)
        Right (Vm.ExecResult vmContext) -> do
          -- putStrLn $ "@[genFileFromTemplate] result: " <> unpack (TE.decodeUtf8 vmContext.outStream)
          BS.writeFile destPath vmContext.outStream
          pure $ Right ()
    ConcatET content -> do
      putStrLn $ "@[genFileFromTemplate] ConcatET templ: " <> show destPath
      BS.writeFile destPath content
      pure $ Right ()
    Jinja _ -> do
      putStrLn $ "@[genFileFromTemplate] Jinja templ: " <> show destPath
      -- TMP: execute the VM on the FileTemplate produced (can be Hugo, Haskell-dant, etc):
      pure $ Right ()
    EndExec -> do
      putStrLn $ "@[genFileFromTemplate] EndExec: " <> show destPath
      pure $ Right ()
    -- execute the VM on the FileTemplate produced (can be Hugo, Haskell-dant, etc):
-- with a Right template parsing, execute the VM on the FileTemplate produced (can be Hugo, Haskell-dant, etc):
  pure $ Right ()
