module SiteDefinition.Explore where

import Control.Monad (forM_)
import qualified Data.Char as DC
import Data.List (isSuffixOf)
-- import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Vector as Vc
import qualified Data.Sequence as Seq
import System.FilePath (joinPath)
import qualified System.Directory.PathWalk as Wlk


data FileItem =
  MarkupFI FilePath
  | HtmlFI FilePath
  | YamlFI FilePath
  | TomlFI FilePath
  | JsonFI FilePath
  deriving (Show)

type RType = Seq.Seq (FilePath, [FileItem])

loadFolderTree :: FilePath -> IO RType
loadFolderTree rootPath = do
  -- putStrLn "@[loadFolderTree] starting."
  -- TODO: run in a try to catch non-existent rootPath
  t <- Wlk.pathWalkAccumulate rootPath filesAnalyser
  pure t
  -- Wlk.pathWalkAccumulate "." filesAnalyser

filesAnalyser :: FilePath -> [FilePath] -> [[Char]] -> IO RType
filesAnalyser root dirs files =
  let
    items = foldl (\accum file ->
                    case itemMaker file of
                      Nothing -> accum
                      Just anItem -> accum <> [anItem]
                    ) [] files
  in
  if length items > 0 then
    pure $ Seq.singleton (root, items)
  else
    pure $ Seq.empty


itemMaker :: FilePath -> Maybe FileItem
itemMaker fileName =
  if ".md" `isSuffixOf` fileName then
    Just $ MarkupFI fileName
  else if ".html" `isSuffixOf` fileName then
    Just $ HtmlFI fileName
  else if ".yaml" `isSuffixOf` fileName then
    Just $ YamlFI fileName
  else if ".toml" `isSuffixOf` fileName then
    Just $ TomlFI fileName
  else if ".json" `isSuffixOf` fileName then
    Just $ JsonFI fileName
  else
    Nothing
