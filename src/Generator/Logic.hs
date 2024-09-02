{-# LANGUAGE LambdaCase #-}
module Generator.Logic where

import Control.Monad (foldM, forM, forM_)

import qualified Data.ByteString as BS
import qualified Data.Foldable as Fld
import qualified Data.Map as Mp
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE

import qualified System.Directory as SE

import Conclusion (GenError (..))
import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..))
import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs
import qualified RunTime.Interpreter as Rt
import qualified Markup.Page as Mrkp
import Markup.Types (MarkupPage (..))
import Template.Haskell (tsParseHaskell)
import qualified Template.Parser as Tmpl
import Template.Types (ProjectTempl (..), FileTempl (..), Function (..), Code (..))
import ProjectDefinition.Types (ProjectDefinition (..), ProjectType (..), SiteType (..))
import qualified ProjectDefinition.AssocRules as Rules
import qualified ProjectDefinition.Hugo as Hu
import RunTime.Interpreter (createContext, execute)
import qualified RunTime.Interpreter.Context as Vm
import qualified RunTime.Interpreter.Engine as Vm

import Generator.Types


type TemplateMatches = Mp.Map FilePath [ MarkupPage ]


{- First draft of logic: -}

createSite :: RunOptions -> IO (Either GenError ())
createSite rtOpts = do
  putStrLn $ "@[createSite] starting, root: " <> rtOpts.baseDir
  -- explore the folder for the site -> flat list of all dirs and files.
  -- eiSiteDef <- Fs.buildDirTree rtOpts
  eiSiteDef <- Fs.loadFolderTree rtOpts.baseDir
  -- extract the content pages, turn them into with Markup.parseContent to [ MarkupPage ]
  case eiSiteDef of
    Left err -> pure . Left $ SimpleMsg (pack . show $ err)
    Right dirTree ->
      let
        contentPages = []   -- Fs.getContentPages siteDef
        siteDef = ProjectDefinition rtOpts.baseDir (Site (Hugo Hu.defaultComponents)) [] dirTree
      in do
        listEiGen <- mapM (Mrkp.parseContent rtOpts) contentPages
        let
          eiTemplateSet = foldM (eiTemplateFinder siteDef) (Mp.empty :: TemplateMatches) listEiGen
          execContext = Rt.createContext rtOpts
        case eiTemplateSet of
          Left err -> pure $ Left err
          Right templateSet -> do
            rez <- foldM (\eiCtxt aTmpl -> case eiCtxt of Left err -> pure $ Left err ; Right aCtxt -> genOutput rtOpts siteDef aCtxt aTmpl) (Right execContext :: Either GenError Rt.ExecContext) (Mp.assocs templateSet)
            case rez of
              Left err -> pure $ Left err
              Right execContext ->
                -- Send back a result that will help the upper layer; maybe the list of all things created?
                pure $ Right ()
  where
  eiTemplateFinder :: ProjectDefinition -> TemplateMatches -> Either GenError MarkupPage -> Either GenError TemplateMatches
  eiTemplateFinder siteDef matchSet genItem =
    case genItem of
      Left err -> Left . SimpleMsg . pack $ "@[createSite] parseContent err: " <> show err
      -- TODO: match the templates for a given MarkupPage item, and consolidate the MarkupPage under a single set of templates -> use a Map Text [ MarkupPage ]
      Right anItem ->
        let
          aTmpl = Rules.findTemplForContent siteDef anItem
        in
        case Mp.lookup aTmpl matchSet of
          Nothing -> Right $ Mp.insert aTmpl [anItem] matchSet
          Just genList -> Right $ Mp.insert aTmpl (anItem : genList) matchSet


genOutput :: RunOptions -> ProjectDefinition -> Rt.ExecContext -> (FilePath, [ MarkupPage ]) -> IO (Either GenError Rt.ExecContext)
genOutput rtOpts siteDef execCtxt (tmplName, genList) = do
  eiTemplate <- Tmpl.parse rtOpts siteDef tmplName
  case eiTemplate of
    Left err -> pure . Left . SimpleMsg $ err
    Right template ->
      foldM (\eiCtxt item -> case eiCtxt of Left err -> pure $ Left err; Right aCtxt -> Rt.execute rtOpts siteDef template aCtxt (Just item)) (Right execCtxt) genList


-- Tests:

fullExploration :: RunOptions -> IO (Either String ())
fullExploration rtOpts = do
  let
    -- folders = ["archetypes", "assets", "config", "content", "data", "layouts", "public", "static", "themes"]
    folders = ["archetypes", "config", "content", "data", "layouts", "static"]
  putStrLn "@[fullExploration] starting."
  fTrees <- forM folders (\subDir -> Fs.loadFolderTree (rtOpts.baseDir <> "/" <> subDir))
  let
    goodTrees = foldl (\accum eif -> case eif of Left _ -> accum ; Right f -> accum <> [f]) [] fTrees
  displayFTrees rtOpts goodTrees
  countItems goodTrees
  -- load markup files
  -- load templates
  -- load theme(s)
  -- for each markup file, execute (config, file, template)
  pure . Right $ ()


displayFTrees :: RunOptions -> [ Seq.Seq (String, [Fs.FileItem]) ] -> IO ()
displayFTrees rtOpts fTrees = do
  putStrLn $ "@[displayFTrees] folder count: " <> (show . length $ fTrees) <> "."
  forM_ fTrees (\fTree ->
      forM_ fTree (\(folder, items) -> do
            putStrLn $ "In folder " <> folder
            forM_ items (\item -> do
                putStrLn $ "  | " <> show item
                case item of
                  {-
                  Fs.TomlFI filePath -> do
                    rez <- Cfgp.parseToml (r <> "/" <> filePath)
                    pure ()
                  -}
                  Fs.KnownFile Fs.Markup filePath -> do
                    Mrkp.parseContent rtOpts (folder <> "/" <> filePath)
                    -- TMP:
                    pure $ Right ()
                  _ -> pure . Left . SimpleMsg . pack $ "@[displayFTrees] unknown item: " <> show item
              )
          )
    )

countItems fTrees =
  let
    totalItems =
      foldl (
        -- outer foldl: counter & a fTree; inner foldl: counter & item pairs.
        foldl (\accum (r, items) -> accum + length items)
      ) 0 fTrees
  in
  putStrLn $ "@[countItems] total: " <> show totalItems <> "."

{- 2nd draft: uses WorkPlan -}

runGen :: RunOptions -> ProjectTempl -> WorkPlan -> IO (Either GenError ())
runGen rtOpts projTempl workPlan = do
  mapM_ (\wi -> do
      putStrLn $ "@[runGen] wi: " <> show wi
      runRez <- runItem rtOpts workPlan.destDir projTempl wi
      case runRez of
        Left err -> do
          let
            errMsg = "@[runGen] runItem err: " <> show err
          putStrLn errMsg
          pure $ Left err
        Right _ -> pure $ Right ()
    ) workPlan.workItems
  pure $ Right ()


runItem :: RunOptions -> FilePath -> ProjectTempl -> WorkItem -> IO (Either GenError ())
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
  DupFromSource fItem srcPath destPath ->
    let
      fullDestPath = destDir <> "/" <> destPath
    in do
    putStrLn $ "@[runItem] DupFromSource: " <> srcPath <> " -> " <> fullDestPath
    alreadyThere <- SE.doesFileExist fullDestPath
    if alreadyThere then
      putStrLn $ "@[runItem] skipping existing CloneSource: " <> fullDestPath
    else
      SE.copyFile srcPath fullDestPath
    pure $ Right ()
  RunTemplate path -> do
    putStrLn $ "@[runItem] RunTemplate: " <> path
    pure $ Right ()
  RunTemplateToDest tKind dir tPath destPath -> do
    case tPath of
        Fs.KnownFile fType srcPath ->
          let
            fullPath = case projTempl.hasPrefix of
              Nothing -> projTempl.path <> "/" <> dir <> "/" <> srcPath
              Just prefix -> projTempl.path <> "/" <> dir <> "/" <> srcPath
          in
            genFileFromTemplate rtOpts tKind fullPath (destDir <> "/" <> destPath)
        Fs.MiscFile srcPath -> do
          putStrLn $ "@[runItem] RunTemplateToDest: " <> show tKind <> ", src: " <> show tPath <> ", dst: " <> destPath
          pure $ Right ()


data ExecTemplate =
  FuddleVM Vm.VMModule
  | Jinja String
  | EndExec
  deriving Show


genFileFromTemplate :: RunOptions -> Fs.FileKind -> FilePath -> FilePath -> IO (Either GenError ())
genFileFromTemplate rtOpts fType srcPath destPath = do
  putStrLn $ "@[genFileFromTemplate] starting: " <> srcPath <> " -> " <> destPath
  -- TODO: figure out a common interface amongst all template files for the VM execution.
  eiFTemplate <- case fType of
    Fs.Haskell -> do
      putStrLn $ "@[runItem] Haskell: " <> show srcPath
      -- read/ts-parse the template file
      rezA <- tsParseHaskell srcPath
      case rezA of
        Left err -> pure $ Left err
        Right fTemplate -> do
          -- TEST:
          case fTemplate.logic of
            [] -> pure $ Right EndExec
            anOp : _ -> case anOp of
              CloneVerbatim _ -> do
                SE.copyFile srcPath destPath
                pure $ Right EndExec
              Exec vmModule -> do
                pure . Right $ FuddleVM vmModule
              _ -> pure . Left . SimpleMsg . pack $ "@[genFileFromTemplate] logic " <> show anOp <> " not implemented."
    Fs.Markup -> do
      putStrLn $ "@[genFileFromTemplate] no engine for: " <> show srcPath
      pure . Left $ SimpleMsg "@[genFileFromTemplate] no engine for markup template."
  case eiFTemplate of
    Left err -> do
      putStrLn $ "@[genFileFromTemplate] eiFtemplate error: " <> show err
      pure $ Left err
    Right aVM -> case aVM of
      FuddleVM vmModule -> do
        putStrLn $ "@[genFileFromTemplate] Haskell templ, executing VM module: " <> show vmModule
        -- TODO: create a runtime context, execute the VM on the FileTemplate produced (can be Hugo, Haskell-dant, etc):
        eiRez <- Vm.execModule vmModule
        case eiRez of
          Left errMsg -> do
            putStrLn $ "@[genFileFromTemplate] VM error: " <> show errMsg
            pure $ Left $ SimpleMsg (pack errMsg)
          Right (Vm.ExecResult vmContext) -> do
            -- putStrLn $ "@[genFileFromTemplate] result: " <> unpack (TE.decodeUtf8 vmContext.outStream)
            BS.writeFile destPath vmContext.outStream
            pure $ Right ()
      Jinja _ -> do
        putStrLn $ "@[genFileFromTemplate] Jinja templ: " <> show srcPath
        -- TMP: execute the VM on the FileTemplate produced (can be Hugo, Haskell-dant, etc):
        pure $ Right ()
      EndExec -> do
        putStrLn $ "@[genFileFromTemplate] EndExec: " <> show srcPath
        pure $ Right ()
      -- execute the VM on the FileTemplate produced (can be Hugo, Haskell-dant, etc):
  -- with a Right template parsing, execute the VM on the FileTemplate produced (can be Hugo, Haskell-dant, etc):
  pure $ Right ()


buildWorkPlan :: RunOptions -> NewOptions -> ProjectTempl -> WorkPlan
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
    Fs.KnownFile Fs.DanTmpl path -> Just $ RunTemplate (buildPath dir path)
    Fs.KnownFile Fs.Haskell path -> Just $ RunTemplateToDest Fs.Haskell dir src (buildPath dir path)
    Fs.KnownFile _ path -> Just $ DupFromSource src (buildPath dir path) (buildPath dir path)


buildPath :: FilePath -> FilePath -> FilePath
buildPath dir src =
  case dir of
    "" -> src
    _ -> dir <> "/" <> src