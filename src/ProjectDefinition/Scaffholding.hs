module ProjectDefinition.Scaffholding where

import Control.Monad (unless, when)
import qualified Data.Foldable as Fld
import qualified System.Directory as SE

import qualified Options.Types as Op
import qualified Options.Runtime as Op
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs

import Generator.Types (WorkPlan (..), WorkItem (..))
import Generator.Work (runGen)
import Template.Types (ScaffholdTempl (..))
import Template.FileTree (mergeTemplates)

createFileTree :: Op.RunOptions -> Op.NewOptions -> [ScaffholdTempl] -> IO (Either GenError ())
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


buildWorkPlan :: Op.RunOptions -> Op.NewOptions -> ScaffholdTempl -> WorkPlan
buildWorkPlan rtOpts newOpts template =
  let
    structList = Fld.toList template.structure
    newDirs = map NewDirIfNotExist (extractDirs structList)
    workItems = concatMap analyzeSources structList
  in
  WorkPlan newOpts.rootDir $ newDirs <> workItems


extractDirs :: [ Fs.PathNode ] -> [ FilePath ]
extractDirs = map fst . filter ((/= "") . fst)


analyzeSources :: Fs.PathNode -> [ WorkItem ]
analyzeSources (dir, srcs) =
    foldl (\accum src ->
      case workForSource dir src of
        Nothing -> accum
        Just workItem -> accum <> [ workItem ]
    ) [] srcs


workForSource :: FilePath -> Fs.FileItem -> Maybe WorkItem
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
