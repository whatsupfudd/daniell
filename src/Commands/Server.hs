module Commands.Server where

import Control.Monad (forM_, forM)

import qualified Data.Sequence as Seq
import qualified Options.RunOptions as Rto

import qualified Conclusion as Ccl
import qualified SiteDefinition.Explore as Expl
import qualified Options.Config as Cfgp
import qualified Markup.Parser as Mrkp

serverHu :: Rto.RunOptions -> IO Ccl.Conclusion
serverHu rtOpts = do
  mbStaticSite <- getStaticSite rtOpts
  case mbStaticSite of
    Left errMsg ->
      putStrLn $ "@[serverHu] err: " <> errMsg
    Right aSite ->
      -- TODO: start web service on the root path to the site.
      putStrLn $ "@[serverHu] happy!"
  pure Ccl.NilCcl


-- Goes in the RunTime system:
data StaticSite = 
  InMemory
  | OnStorage

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
                  Expl.TomlFI filePath -> do
                    rez <- Cfgp.parseToml (r <> "/" <> filePath)
                    pure ()
                  Expl.MarkupFI filePath -> do
                    Mrkp.parseMarkdown (r <> "/" <> filePath)
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
