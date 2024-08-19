{-# LANGUAGE LambdaCase #-}
module Generator.Logic where

import Control.Monad (foldM, forM, forM_)

import qualified Data.Map as Mp
import Data.Text (Text, pack)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fld

import Conclusion (GenError (..))
import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions)
import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs
import qualified RunTime.Interpreter as Rt
import qualified Markup.Page as Mrkp
import Markup.Types (MarkupPage (..))
import Template.Haskell (testTreeSitter)
import qualified Template.Parser as Tmpl
import Template.Types (ProjectTempl (..))
import ProjectDefinition.Types (ProjectDefinition (..), ProjectType (..), SiteType (..))
import qualified ProjectDefinition.AssocRules as Rules
import qualified ProjectDefinition.Hugo as Hu
import RunTime.Interpreter (createContext, execute)

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

runGen :: RunOptions -> ProjectTempl -> [ WorkItem ] -> IO (Either GenError ())
runGen rtOpts projTempl workPlan = do
  mapM_ (\wi -> do
      putStrLn $ "@[runGen] wi: " <> show wi
      runItem rtOpts projTempl wi
    ) workPlan
  pure $ Right ()


runItem :: RunOptions -> ProjectTempl -> WorkItem -> IO (Either GenError ())
runItem rtOpts projTempl = \case
  NewDirIfNotExist dirPath -> do
    putStrLn $ "@[runItem] NewDir: " <> dirPath
    pure $ Right ()
  CloneSource srcPath destPath -> do
    putStrLn $ "@[runItem] CloneSource: " <> srcPath <> " -> " <> destPath
    pure $ Right ()
  DupFromSource fItem srcPath destPath -> do
    putStrLn $ "@[runItem] DupFromSource: " <> srcPath <> " -> " <> destPath
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
          in do
            putStrLn $ "@[runItem] treeSitting: " <> show tPath
            -- HERE: read the result of the code template parsing, and execute the VM on that + local context.
            testTreeSitter fullPath
            -- createContext
            -- execute:: RunOptions -> ProjectDefinition -> Template -> ExecContext -> MarkupPage
        Fs.MiscFile srcPath ->
          putStrLn $ "@[runItem] RunTemplateToDest: " <> show tKind <> ", src: " <> show tPath <> ", dst: " <> destPath

    pure $ Right ()


buildWorkPlan :: RunOptions -> NewOptions -> ProjectTempl -> [ WorkItem ]
buildWorkPlan rtOpts newOpts template =
  let
    structList = Fld.toList template.structure
    newDirs = map NewDirIfNotExist $ extractDirs structList
    workItems = concatMap analyzeSources structList
  in
  newDirs <> workItems


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