module ProjectDefinition.Scaffolding where

import Control.Monad (unless, when)
import qualified Data.Foldable as Fld
import qualified System.Directory as SE

import qualified Options.Types as Op
import qualified Options.Runtime as Op
import Conclusion (Conclusion (..), GenError (..))
import qualified FileSystem.Types as Fs

import Generator.Types (WorkPlan (..), WorkItem (..), ScfWorkItem (..))
import Generator.Work (ScfWorkPlan, ScfEngine (..), ScfContext (..), runGen)
import Template.Types (ScaffoldTempl (..))
import Template.FileTree (mergeTemplates, loadTree)

import Utils (splitResults)

import ProjectDefinition.Defaults (defaultLocations)


createScaffolding :: Op.RunOptions -> Op.NewOptions -> IO Conclusion
createScaffolding rtOpts newOpts = do
  rezTemplates <- mapM (parseFileTree rtOpts) newOpts.templates
  let
    (errTemplates, userTemplates) = splitResults rezTemplates
  case errTemplates of
    -- No errors, keep going.
    [] -> do
      -- putStrLn $ "Parsed templates: " <> show userTemplates
      -- if there's no 'no-default template' instruction in the specified templates, load the default template.
      rezA <-
        -- TODO: figure out when to not scan the defaultLocations...
        --  if False then pure $ Right userTemplates else
        do
        rezB <- parseFileTree rtOpts (defaultLocations rtOpts newOpts.projKind)
        case rezB of
          Left errMsg -> pure . Left $ show errMsg
          Right defTempl -> pure . Right $ userTemplates <> [ defTempl ]
      case rezA of
        Left errMsg -> pure $ ErrorCcl $ "@[createScaffolding] error loading default template: " <> show errMsg
        Right allTemplates -> do
          rezB <- createFileTree rtOpts newOpts allTemplates
          case rezB of
            Left errMsg -> pure $ ErrorCcl $ "@[createScaffolding] error creating project: " <> show errMsg
            Right _ -> pure NilCcl
    -- Errors while reading templates, abort.
    _ -> do
      putStrLn $ "@[createScaffolding] Template loading error: " <> show errTemplates
      pure $ ErrorCcl $ "@[createScaffolding] error loading templates: " <> show errTemplates


parseFileTree :: Op.RunOptions -> FilePath -> IO (Either GenError ScaffoldTempl)
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


createFileTree :: Op.RunOptions -> Op.NewOptions -> [ScaffoldTempl] -> IO (Either GenError ())
createFileTree rtOpts newOpts templates = do
  putStrLn "@[createFileTree] starting."
  -- move template(s) info into a work plan.
  let
    mergedTemplate = mergeTemplates templates
    workPlan = buildWorkPlan rtOpts newOpts mergedTemplate
  -- pass the work plan to the generator.
  destDirExist <- SE.doesDirectoryExist newOpts.rootDir
  unless destDirExist $ SE.createDirectory newOpts.rootDir
  runGen rtOpts mergedTemplate workPlan


buildWorkPlan :: Op.RunOptions -> Op.NewOptions -> ScaffoldTempl -> ScfWorkPlan
buildWorkPlan rtOpts newOpts template =
  let
    structList = Fld.toList template.structure
    newDirs = map NewDirIfNotExist (extractDirs structList)
    workItems = concatMap analyzeSources structList
  in
  WorkPlan {
      destDir = newOpts.rootDir
    , items = newDirs <> workItems
    , engine = ScfEngine
    , context = ScfContext
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
