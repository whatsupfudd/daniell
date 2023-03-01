module Generator.Logic where

import qualified Data.Map as Mp
import Data.Text (Text, pack)

import Control.Monad (foldM, forM, forM_)
import Conclusion (GenError (..))
import Markup.Types (MarkupPage (..))
import Options.RunOptions (RunOptions (..))
import qualified SiteDefinition.Explore as Expl
import SiteDefinition.Types (SiteDefinition (..))
import qualified SiteDefinition.AssocRules as Rules
import qualified RunTime.Interpreter as Rt
import qualified Markup.Page as Mrkp
import qualified Template.Parser as Tmpl


type TemplateMatches = Mp.Map FilePath [ MarkupPage ]


createSite :: RunOptions -> IO (Either GenError ())
createSite rtOpts = do
  putStrLn $ "@[createSite] starting, root: " <> rtOpts.baseDir
  -- explore the folder for the site -> SiteDefinition
  eiSiteDef <- Expl.buildGenDef rtOpts
  -- extract the content pages, turn them into with Markup.parseContent to [ MarkupPage ]
  case eiSiteDef of
    Left err -> pure . Left $ SimpleMsg (pack . show $ err)
    Right siteDef ->
      let
        -- contentPages :: [ File{ath ]
        contentPages = Expl.getContentPages siteDef
      in do
        -- Mrkp.parseContent rtOpts -> IO (Either GenError MarkupPage )
        --  => mapM = IO [ Either GenError MarkupPage ] ; listEiGen :: [ Either GenError MarkupPage ]
        listEiGen <- mapM (Mrkp.parseContent rtOpts) contentPages
        let
          -- templateSet :: Either GenError TemplateMatches; (mapM :: (a -> m b) -> t a -> m (t b))
          eiTemplateSet = foldM (eiTemplateFinder siteDef) (Mp.empty :: TemplateMatches) listEiGen
          -- execContext :: Rt.ExecContext
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
  eiTemplateFinder :: SiteDefinition -> TemplateMatches -> Either GenError MarkupPage -> Either GenError TemplateMatches
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


genOutput :: RunOptions -> SiteDefinition -> Rt.ExecContext -> (FilePath, [ MarkupPage ]) -> IO (Either GenError Rt.ExecContext)
genOutput rtOpts siteDef execCtxt (tmplName, genList) = do
  eiTemplate <- Tmpl.parse rtOpts siteDef tmplName
  case eiTemplate of
    Left err -> pure . Left . SimpleMsg $ err
    Right template -> do
      -- execute :: RunOptions -> SiteDefinition -> Template -> ExecContext -> MarkupPage -> IO (Either GenError ExecContext)
      rez <- foldM (\eiCtxt item -> case eiCtxt of Left err -> pure $ Left err; Right aCtxt -> Rt.execute rtOpts siteDef template aCtxt item) (Right execCtxt) genList
      pure rez



-- Tests:

fullExploration :: RunOptions -> IO (Either String ())
fullExploration rtOpts = do
  let
    -- folders = ["archetypes", "assets", "config", "content", "data", "layouts", "public", "static", "themes"]
    folders = ["archetypes", "config", "content", "data", "layouts", "static"]
  putStrLn "@[fullExploration] starting."
  fTrees <- forM folders (\subDir -> Expl.loadFolderTree (rtOpts.baseDir <> "/" <> subDir))
  displayFTrees rtOpts fTrees
  countItems fTrees
  -- load markup files
  -- load templates
  -- load theme(s)
  -- for each markup file, execute (config, file, template)
  pure . Right $ ()


displayFTrees rtOpts fTrees = do
  putStrLn $ "@[displayFTrees] folder count: " <> (show . length $ fTrees) <> "."
  forM_ fTrees (\fTree ->
      forM_ fTree (\(r, items) -> do
            putStrLn $ "In folder " <> r
            forM_ items (\item -> do
                putStrLn $ "  | " <> show item
                case item of
                  {-
                  Expl.TomlFI filePath -> do
                    rez <- Cfgp.parseToml (r <> "/" <> filePath)
                    pure ()
                  -}
                  Expl.MarkupFI filePath -> do
                    Mrkp.parseContent rtOpts (r <> "/" <> filePath)
                    -- TMP:
                    pure $ Right ()
                  _ -> pure . Left . SimpleMsg . pack $ "@[displayFTrees] unknown item: " <> show item
              )
          )
    )

countItems fTrees =
  let
    totalItems = 
      foldl (\accum fTree ->
          foldl (\accum (r, items) -> accum + length items) accum fTree
      ) 0 fTrees
  in
  putStrLn $ "@[countItems] total: " <> show totalItems <> "."
