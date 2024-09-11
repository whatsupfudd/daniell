{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Generator.Logic where

import Control.Monad (foldM, forM, forM_)

import qualified Data.ByteString as BS
import qualified Data.Map as Mp
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE

import qualified System.Directory as SE

import Conclusion (GenError (..), Conclusion (..))
import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..), ProjectKind (..), SiteOptions (..))
import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs
import ProjectDefinition.Types (ProjectDefinition (..), ProjectType (..), SiteType (..), WebAppType (..), LocalAppType (..))
import qualified ProjectDefinition.AssocRules as Rules
import qualified ProjectDefinition.Hugo as Hu
import ProjectDefinition.Hugo.Config (setRunOptions)
import qualified ProjectDefinition.NextJS as Nx
import qualified ProjectDefinition.Scaffholding as Scf
import ProjectDefinition.Defaults (defaultLocations)
import Template.Haskell (tsParseHaskell)
import qualified Template.Parser as Tmpl
import Template.Types (ScaffholdTempl (..), FileTempl (..), Function (..), Code (..))
import qualified Markup.Page as Mrkp
import Markup.Types (MarkupPage (..))
import qualified RunTime.Interpreter as Ri
import qualified RunTime.Interpreter.Context as Vm
import qualified RunTime.Interpreter.Engine as Vm

import Utils (splitResults)
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

{- New stuff 24.09.02 -}
buildSite :: RunOptions -> SiteOptions -> IO (Either GenError ())
buildSite rtOpts siteOpts = do
  putStrLn $ "@[buildSite] starting, root: " <> rtOpts.baseDir
  -- explore the folder for the site -> flat list of all dirs and files.
  -- eiSiteDef <- Fs.buildDirTree rtOpts
  -- TODO: need to check the HugoBuildOptions (configDir, dataDir, ...) and
  --   assemble the file list based on a loadFolderTree of these locations.
  eiSiteDef <- Fs.loadFolderTree rtOpts.baseDir
  putStrLn $ "@[buildSite] eiSiteDef: " <> show eiSiteDef
  -- extract the content pages, turn them into with Markup.parseContent to [ MarkupPage ]
  case eiSiteDef of
    Left err -> pure . Left $ SimpleMsg (pack . show $ err)
    Right dirTree -> do
      -- TODO: move the dirTree analysis in ProjectDefinition.Logic.
      eiWorkPlan <-
         case siteOpts of
          FuddleSS -> pure $ analyseFuddleProject rtOpts True dirTree
          GatsbySS -> pure $ analyseGatsbyProject rtOpts dirTree
          HugoSS hugoOpts ->
            let
              newRtOpts = setRunOptions rtOpts hugoOpts
            in
            Hu.analyseProject newRtOpts dirTree
          NextSS -> pure $ Nx.analyseNextJsProject rtOpts True dirTree
          _ -> pure . Left . SimpleMsg . pack $ "@[buildSite] unknown subproject kind: " <> show siteOpts
      case eiWorkPlan of
        Left err -> pure $ Left err
        Right workPlan -> do
          pure $ Right ()
  where
  eiTemplateFinder :: ProjectDefinition -> TemplateMatches -> Either GenError MarkupPage -> Either GenError TemplateMatches
  eiTemplateFinder siteDef matchSet genItem =
    case genItem of
      Left err -> Left . SimpleMsg . pack $ "@[buildSite] parseContent err: " <> show err
      -- TODO: match the templates for a given MarkupPage item, and consolidate the MarkupPage under a single set of templates -> use a Map Text [ MarkupPage ]
      Right anItem ->
        let
          aTmpl = Rules.findTemplForContent siteDef anItem
        in
        case Mp.lookup aTmpl matchSet of
          Nothing -> Right $ Mp.insert aTmpl [anItem] matchSet
          Just genList -> Right $ Mp.insert aTmpl (anItem : genList) matchSet


-- TODO: move all the analyse... logic to their respective module in ProjectDefinition.

analyseGatsbyProject :: RunOptions -> Fs.PathFiles -> Either GenError WorkPlan
analyseGatsbyProject rtOpts pathFiles =
  -- TODO:
  Left $ SimpleMsg "Gatsby project not implemented yet."

analyseFuddleProject :: RunOptions -> Bool -> Fs.PathFiles -> Either GenError WorkPlan
analyseFuddleProject rtOpts isStatic pathFiles =
  -- TODO:
  Left $ SimpleMsg "Fuddle project not implemented yet."


{- 2nd draft: reorg and based on WorkPlan. -}

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
      createScaffholding rtOpts newOpts


createScaffholding :: RunOptions -> NewOptions -> IO Conclusion
createScaffholding rtOpts newOpts = do
  rezTemplates <- mapM (Scf.parseFileTree rtOpts) newOpts.templates
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
        rezB <- Scf.parseFileTree rtOpts (defaultLocations rtOpts newOpts.projKind)
        case rezB of
          Left errMsg -> pure . Left $ show errMsg
          Right defTempl -> pure . Right $ userTemplates <> [ defTempl ]
      case rezA of
        Left errMsg -> pure $ ErrorCcl $ "@[newCmd] error loading default template: " <> show errMsg
        Right allTemplates -> do
          rezB <- Scf.createFileTree rtOpts newOpts allTemplates
          case rezB of
            Left errMsg -> pure $ ErrorCcl $ "@[newCmd] error creating project: " <> show errMsg
            Right _ -> pure NilCcl
    -- Errors while reading templates, abort.
    _ -> do
      putStrLn $ "@[newCmd] Template loading error: " <> show errTemplates
      pure $ ErrorCcl $ "@[newCmd] error loading templates: " <> show errTemplates


