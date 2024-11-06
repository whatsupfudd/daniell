{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Generator.Logic where

import Control.Monad (foldM, forM, forM_)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import qualified Data.Map as Mp
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE

import qualified System.Directory as SE

import Conclusion (GenError (..), Conclusion (..))
import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..), ProjectKind (..), SiteOptions (..), PhpBuildOptions (..), WebAppOptions (..))
import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs
import ProjectDefinition.Types (ProjectDefinition (..), ProjectType (..), SiteType (..), WebAppType (..), LocalAppType (..))
import qualified ProjectDefinition.AssocRules as Rules
import qualified ProjectDefinition.Hugo as Hu
import qualified ProjectDefinition.Hugo.Config as Hu
import qualified ProjectDefinition.Hugo.Types as Ht
import qualified ProjectDefinition.NextJS as Nx
import qualified ProjectDefinition.Gatsby as Gb
import qualified ProjectDefinition.Fuddle as Fd
import qualified Generator.Work as Scf
import ProjectDefinition.Scaffolding (createScaffolding)
import ProjectDefinition.Gatsby (analyseGatsbyProject)
import ProjectDefinition.Fuddle (analyseFuddleProject)
import Template.Haskell (tsParseHaskell)
import Cannelle.PHP.Parse (tsParsePhp)
import qualified Template.Parser as Tmpl
import qualified Markup.Page as Mrkp
import Markup.Types (MarkupPage (..))
import qualified Cannelle.VM.Context as Vc
import qualified Cannelle.VM.Engine as Vm

import Utils ((>>=?))

import qualified RunTime.Interpreter as Ri
import Generator.Types (ExecSystem (..), WorkPlan (..), ScfWorkPlan (..))
import qualified Generator.Types as Scf


type TemplateMatches = Mp.Map FilePath [ MarkupPage ]


{-
doPlan :: RunOptions -> SpecPlan -> IO (Either GenError GenericContext)
doPlan rtOpts plan =
  case plan of
    ScfPlan scfPlan -> (ScfContext <$>) <$> runWP rtOpts scfPlan
    GatsbyPlan gbPlan -> (GatsbyContext <$>) <$> runWP rtOpts gbPlan
    HugoPlan hgPlan -> (HugoContext <$>) <$> runWP rtOpts hgPlan
    NextPlan nxPlan ->  (NextContext <$>) <$> runWP rtOpts nxPlan
    FuddlePlan fdPlan -> (FuddleContext <$>) <$> runWP rtOpts fdPlan
  where
  runWP :: (Engine e, Context c, Show c, WorkItem w) => RunOptions -> WorkPlan e c w -> IO (Either GenError c)
  runWP rtOpts workPlan = do
    putStrLn $ "@[run] workPlan: " <> show workPlan
    runEng rtOpts workPlan.engine workPlan.context workPlan.items
  work :: (Engine e, Context c, WorkItem w) => RunOptions -> e -> c -> [ w ] -> IO (Either GenError c)
  work rtOpts engine context =
    foldM (\eiCtxt aItem -> case eiCtxt of
        Left err -> pure $ Left err
        Right aCtxt -> execItem rtOpts engine aCtxt aItem
      ) (Right context)
  execItem :: (Engine e, Context c, WorkItem w) => RunOptions -> e -> c -> w -> IO (Either GenError c)
  execItem rtOpts engine context item = do
    run engine context
    pure $ Right context
-}

data SpecPlan =
  ScfPlan ScfWorkPlan
  | GatsbyPlan Gb.GbWorkPlan
  | HugoPlan Ht.HgWorkPlan
  | NextPlan Nx.NxWorkPlan
  | FuddlePlan Fd.FdWorkPlan
  | PhpPlan
  deriving Show


buildSite :: RunOptions -> SiteOptions -> IO (Either GenError ())
buildSite rtOpts siteOpts = do
  putStrLn $ "@[buildSite] starting, root: " <> rtOpts.baseDir
  {- TODO: revise this approach to:
    - define runtime options for the tech based on cli/conf-file/env vars.
    - do a first pass at loading the project definition (normally, only scan for config files in the main dir as they can define other locations to use).
    - analyse the project completely, producing a work plan.
    - configure an output context.
    - execute the work plan.
    - report on errors that occured during the whole process.
  -}
  eiWorkPlan <- case siteOpts of
    FuddleSS -> (FuddlePlan <$>) <$> analyseFuddleProject rtOpts True Seq.empty
    GatsbySS -> (GatsbyPlan <$>) <$> analyseGatsbyProject rtOpts Seq.empty
    HugoSS hugoOpts ->
      let
        newRtOpts = Hu.setRunOptions rtOpts hugoOpts
      in do
      eiSiteDef <- Fs.loadFolderTree rtOpts.baseDir
      putStrLn $ "@[buildSite] eiSiteDef:\n" <> showSiteDef eiSiteDef
      case eiSiteDef of
        Left err -> pure . Left $ SimpleMsg (pack . show $ err)
        Right dirTree -> (HugoPlan <$>) <$> Hu.analyseProject newRtOpts dirTree
    NextSS -> do
      eiSiteDef <- Fs.loadFolderTree rtOpts.baseDir
      putStrLn $ "@[buildSite] eiSiteDef:\n" <> showSiteDef eiSiteDef
      case eiSiteDef of
        Left err -> pure . Left $ SimpleMsg (pack . show $ err)
        Right dirTree -> (NextPlan <$>) <$> Nx.analyseProject rtOpts True dirTree
    PhpSS phpOpts ->
      -- TODO: add the proper execution of a PHP WP & Laravel template.
      case phpOpts.srcDir of
        Nothing -> pure . Left . SimpleMsg . pack $ "@[buildSite] no srcDir for Php project."
        Just aText ->
          let
            debugMode = False
          in do
          tsParsePhp debugMode $ unpack aText
          pure $ Right PhpPlan
    _ -> pure . Left . SimpleMsg . pack $ "@[buildSite] unknown subproject kind: " <> show siteOpts

  case eiWorkPlan of
    Left err -> pure $ Left err
    Right techPlan -> do
      -- putStrLn $ "@[buildSite] workPlan: " <> show techPlan
      case techPlan of
        ScfPlan scfPlan -> do
          putStrLn $ "@[buildSite] scaffold workPlan: " <> show scfPlan
          rezA <- runPlan rtOpts scfPlan.engine scfPlan.context scfPlan.items
          case rezA of
            Left err -> pure $ Left err
            Right _ -> pure $ Right ()
        GatsbyPlan gbPlan -> do
          putStrLn $ "@[buildSite] gatsby workPlan: " <> show gbPlan
          rezA <- runPlan rtOpts gbPlan.engine gbPlan.context gbPlan.items
          case rezA of
            Left err -> pure $ Left err
            Right _ -> pure $ Right ()
        HugoPlan hgPlan -> do
          putStrLn $ "@[buildSite] will run hugo workPlan: " <> show hgPlan
          rezA <- runPlan rtOpts hgPlan.engine hgPlan.context hgPlan.items
          case rezA of
            Left err -> pure $ Left err
            Right _ -> pure $ Right ()
        NextPlan nxPlan -> do
          putStrLn $ "@[buildSite] nextjs workPlan: " <> show nxPlan
          rezA <- runPlan rtOpts nxPlan.engine nxPlan.context nxPlan.items
          case rezA of
            Left err -> pure $ Left err
            Right _ -> pure $ Right ()
        FuddlePlan fdPlan -> do
          putStrLn $ "@[buildSite] fuddle workPlan: " <> show fdPlan
          rezA <- runPlan rtOpts fdPlan.engine fdPlan.context fdPlan.items
          case rezA of
            Left err -> pure $ Left err
            Right _ -> pure $ Right ()
        PhpPlan -> do
          pure $ Right ()


createProject :: RunOptions -> NewOptions -> IO Conclusion
createProject rtOpts newOpts =
  case newOpts.projKind of
    SitePK -> do
      putStrLn "@[createProject] Site project."
      pure NilCcl
    WebAppPK -> do
      putStrLn "@[createProject] WebApp project."
      pure NilCcl
    LocalAppPK ->
      createScaffolding rtOpts newOpts


buildWebApp :: RunOptions -> WebAppOptions -> IO (Either GenError ())
buildWebApp rtOpts waOpts = do
  putStrLn "@[buildWebApp] starting."
  case waOpts of
    FuddleWA -> pure . Left . SimpleMsg . pack $ "@[buildWebApp] Fuddle not supported yet."
    NextWA nextOpts -> do
      eiSiteDef <- Fs.loadFolderTree rtOpts.baseDir
      putStrLn $ "@[buildWebApp] eiSiteDef:\n" <> showSiteDef eiSiteDef
      case eiSiteDef of
        Left err -> pure . Left $ SimpleMsg (pack . show $ err)
        Right dirTree -> (NextPlan <$>) <$> Nx.analyseProject rtOpts True dirTree
      >>=? \workPlan -> do
        putStrLn $ "@[buildWebApp] workPlan: " <> show workPlan
        pure . Right $ ()

{- Early stage testing of stuff: -}

serveSite :: RunOptions -> IO (Either GenError ())
serveSite rtOpts = do
  putStrLn $ "@[createSite] starting, root: " <> rtOpts.baseDir
  -- explore the folder for the site -> flat list of all dirs and files.
  eiSiteDef <- Fs.loadFolderTree rtOpts.baseDir
  -- extract the content pages, turn them into with Markup.parseContent to [ MarkupPage ]
  case eiSiteDef of
    Left err -> pure . Left $ SimpleMsg (pack . show $ err)
    Right dirTree ->
      let
        contentPages = []
        siteDef = ProjectDefinition rtOpts.baseDir (Site Hugo) [] dirTree
      in do
        listEiGen <- mapM (Mrkp.parseContent rtOpts rtOpts.baseDir) contentPages
        let
          eiTemplateSet = foldM (eiTemplateFinder siteDef) (Mp.empty :: TemplateMatches) listEiGen
          execContext = Ri.createContext rtOpts
        case eiTemplateSet of
          Left err -> pure $ Left err
          Right templateSet -> do
            rez <- foldM (\eiCtxt aTmpl -> case eiCtxt of Left err -> pure $ Left err ; Right aCtxt -> genOutput rtOpts siteDef aCtxt aTmpl) (Right execContext :: Either GenError Ri.ExecContext) (Mp.assocs templateSet)
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


genOutput :: RunOptions -> ProjectDefinition -> Ri.ExecContext -> (FilePath, [ MarkupPage ]) -> IO (Either GenError Ri.ExecContext)
genOutput rtOpts siteDef execCtxt (tmplName, genList) = do
  eiTemplate <- Tmpl.parse rtOpts siteDef tmplName
  case eiTemplate of
    Left err -> pure . Left . SimpleMsg $ err
    Right template ->
      foldM (\eiCtxt item -> case eiCtxt of
          Left err -> pure $ Left err
          Right aCtxt -> Ri.execute rtOpts siteDef template aCtxt (Just item)
        ) (Right execCtxt) genList


showSiteDef :: Either String Fs.PathFiles -> String
showSiteDef eiSiteDef = case eiSiteDef of
    Left err -> show err
    Right aSiteDef -> concatMap (\(dirPath, items) -> "\t- " <> dirPath <> " -> " <> show items <> "\n") aSiteDef


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
                  Fs.KnownFile Fs.Markdown filePath -> do
                    Mrkp.parseContent rtOpts folder (folder, item)
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
