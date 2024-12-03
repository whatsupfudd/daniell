module ProjectDefinition.Scaffold where

import Control.Monad (unless, when)
import qualified Data.Foldable as Fld
import Data.List.NonEmpty (NonEmpty (..))
import qualified System.Directory as SE

import qualified Options.Types as Op
import qualified Options.Runtime as Op
import Conclusion (Conclusion (..), GenError (..))
import qualified FileSystem.Types as Fs

import Generator.Types (ExecSystem (..), WorkPlan (..))
import Scaffold.Types (ScaffoldBundle (..))
import Scaffold.FileTree (mergeBundles, loadTree)

import Utils (splitResults)

import ProjectDefinition.Defaults (defaultLocations)
import ProjectDefinition.Scaffold.Types
import ProjectDefinition.Scaffold.Work (runItem)  -- For runPlan.

createScaffold :: Op.RunOptions -> Op.NewOptions -> IO Conclusion
createScaffold rtOpts newOpts = do
  rezTemplates <- mapM (parseFileTree rtOpts) newOpts.templates
  let
    (errTemplates, userTemplates) = splitResults rezTemplates
  case errTemplates of
    -- No errors, keep going.
    [] -> do
      -- putStrLn $ "Parsed templates: " <> show userTemplates
      -- if there's no 'no-default template' instruction in the specified templates, load the default template.
      rezA <- do
        -- TODO: figure out when to not scan the defaultLocations...
        --  if False then pure $ Right userTemplates else
        rezB <- parseFileTree rtOpts (defaultLocations rtOpts newOpts.projKind)
        case rezB of
          Left errMsg -> pure . Left $ show errMsg
          Right defTempl -> pure . Right $ userTemplates <> [ defTempl ]
      case rezA of
        Left errMsg -> pure $ ErrorCcl $ "@[createScaffold] error loading default template: " <> show errMsg
        Right allTemplates -> do
          rezC <- createFileTree rtOpts newOpts allTemplates
          case rezC of
            Left errMsg -> pure $ ErrorCcl $ "@[createScaffold] error creating project: " <> show errMsg
            Right _ -> pure NilCcl
    -- Errors while reading templates, abort.
    _ -> do
      putStrLn $ "@[createScaffold] Template loading error: " <> show errTemplates
      pure $ ErrorCcl $ "@[createScaffold] error loading templates: " <> show errTemplates


parseFileTree :: Op.RunOptions -> FilePath -> IO (Either GenError ScaffoldBundle)
parseFileTree rtOpts tPath =
  let
    (fullPath, mbPrefix) = case tPath of
      '.' : rest -> (tPath, Nothing)
      '/' : rest -> (tPath, Nothing)
      _ -> (rtOpts.templateDir <> "/" <> tPath, Just rtOpts.templateDir)
  in do
  rezA <- loadTree rtOpts fullPath
  case rezA of
    Left errMsg -> pure $ Left $ SimpleMsg errMsg
    Right aTempl -> pure $ Right aTempl { hasPrefix = mbPrefix }


createFileTree :: Op.RunOptions -> Op.NewOptions -> [ScaffoldBundle] -> IO (Either GenError ())
createFileTree rtOpts newOpts bundles = do
  putStrLn "@[createFileTree] starting."
  -- move template(s) info into a work plan.
  let
    mergedBundle = mergeBundles bundles
    eiWorkPlan = buildWorkPlan rtOpts newOpts mergedBundle
  case eiWorkPlan of
    Right workPlan -> do
      -- pass the work plan to the generator.
      destDirExist <- SE.doesDirectoryExist newOpts.rootDir
      unless destDirExist $ SE.createDirectory newOpts.rootDir
      -- runGen rtOpts mergedTemplate workPlan
      rezA <- runPlan rtOpts workPlan.engine workPlan.context workPlan.items
      pure $ case rezA of
        Left err -> Left err
        Right _ -> Right ()
    Left err -> pure $ Left err


buildWorkPlan :: Op.RunOptions -> Op.NewOptions -> ScaffoldBundle -> Either GenError ScfWorkPlan
buildWorkPlan rtOpts newOpts bundle =
  let
    structList = Fld.toList bundle.structure
    newDirs = map NewDirIfNotExist (extractDirs structList)
    workItems = concatMap analyzeSources structList
  in
  case newDirs <> workItems of
    [] -> Left $ SimpleMsg "@[buildWorkPlan] no work items."
    h : t -> Right $ WorkPlan {
        items = h :| t
        , engine = ScfEngine
        , context = ScfContext bundle newOpts.rootDir
      }


extractDirs :: [ Fs.PathNode ] -> [ FilePath ]
extractDirs = map fst . filter ((/= "") . fst)


analyzeSources :: Fs.PathNode -> [ ScfWorkItem ]
analyzeSources (dir, srcs) =
    foldl (\accum src ->
      case workForSource dir src of
        Nothing -> accum
        Just workItem -> accum <> [ workItem ]
    ) [] srcs


workForSource :: FilePath -> Fs.FileItem -> Maybe ScfWorkItem
workForSource dir src =
  case src of
    Fs.MiscFile srcPath -> Just $ CloneSource (buildPath dir srcPath) (buildPath dir srcPath)
    Fs.KnownFile fileType path ->
      case fileType of
        Fs.DanTmpl -> Just $ RunTemplate (buildPath dir path)
        _ -> Just $ RunTemplateToDest fileType dir src (buildPath dir path)


buildPath :: FilePath -> FilePath -> FilePath
buildPath dir src =
  case dir of
    "" -> src
    _ -> dir <> "/" <> src
