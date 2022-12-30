module Generator.Logic where

import RunOptions (RunOptions)
import Conclusion (GenError (..))
import qualified SiteDefinition.Explore as Expl
import qualified SiteDefinition.AssocRules as Rule
import qualified RunTime.Interpreter as Rt


createSite :: RunOptions -> IO (Either GenError ())
createSite rtOpts = do
  -- explore the folder for the site -> SiteDefinition
  eiSiteDef <- Expl.buildGenDef rtOpts
  -- extract the content pages, turn them into with Markup.parseContent to [ ContentGen ]
  case eiSiteDef of
    Left err -> pure $ Left err
    Right siteDef ->
      let
        contentPages = Expl.getContentPages siteDef
      in do
        eiGenList <- mapM (Mrkp.parseContent rtOpts) contentPages
        case eiGenList of
          Left err -> pure $ Left err
          Right genList ->
            let
              templateSet = map (Rule.findTemplForContent siteDef) genList
              execContext = Rt.createContext rtOpts
            in do
            rez <- foldM (genOutput rtOpts siteDef) execContext templateSet
            case rez of
              Left err -> pure $ Left err
              Right execContext ->
                -- Send back a result that will help the upper layer; maybe the list of all things created?
                pure $ Right ()


genOuput :: RunOptions -> Expl.SiteDefinition -> -> Rt.ExecContext -> (Text, [ ContentGen ]) -> IO (Either GenError Rt.ExecContext)
genOuput rtOpts siteDef execCtxt (tmplName, genList) = do
  eiTemplate <- Tmpl.parse rtOpts siteDef tmplName
  case eiTemplate of
    Left err -> pure $ Left err
    Right template -> do
      rez <- foldM (Rt.execute rtOpts siteDef template) execCtxt genList
      pure rez



-- Tests:

fullExploration :: RunOptions -> IO (Either String ())
fullExploration rtOpts = do
  let
    -- folders = ["archetypes", "assets", "config", "content", "data", "layouts", "public", "static", "themes"]
    folders = ["archetypes", "config", "content", "data", "layouts", "static"]
  putStrLn "@[fullExploration] starting."
  fTrees <- forM folders (\subDir -> Expl.loadFolderTree (rtOpts.baseDir <> "/" <> subDir))
  displayFTrees fTrees
  countItems fTrees
  -- load markup files
  -- load templates
  -- load theme(s)
  -- for each markup file, execute (config, file, template)
  pure . Right $ ()


displayFTrees fTrees = do
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
                    Mrkp.parse (r <> "/" <> filePath)
                  _ -> pure ()
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
