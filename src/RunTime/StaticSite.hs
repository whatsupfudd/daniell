module RunTime.StaticSite where

import qualified Options.RunOptions as Rto
import qualified Options.Config as Cfgp
import qualified SiteDefinition.Explore as Expl
import qualified Markup.Markdown as Mrkp


data StaticSite = 
  InMemory
  | OnStorage


{-
getStaticSite :: Rto.RunOptions -> IO (Either String StaticSite)
getStaticSite rtOpts = do
  let
    -- folders = ["archetypes", "assets", "config", "content", "data", "layouts", "public", "static", "themes"]
    folders = ["archetypes", "config", "content", "data", "layouts", "static"]
  putStrLn "@[serverHu] starting."
  fTrees <- forM folders (\subDir -> Expl.loadFolderTree (rtOpts.baseDir <> "/" <> subDir))
  displayFTrees fTrees
  countItems fTrees
  -- load markup files
  -- load templates
  -- load theme(s)
  -- for each markup file, execute (config, file, template)
  pure . Right $ InMemory


displayFTrees fTrees = do
  putStrLn $ "@[serverHu] folder count: " <> (show . length $ fTrees) <> "."
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
-}
