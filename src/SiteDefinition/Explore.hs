module SiteDefinition.Explore where

import Control.Monad (forM_)
import qualified Control.Exception as Cexc
import qualified Data.Char as DC
import Data.List (isSuffixOf)
-- import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Vector as Vc
import qualified Data.Sequence as Seq
import qualified System.Directory.PathWalk as Wlk
import System.FilePath (joinPath)
import qualified System.IO.Error as Serr


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
  mbRez <- Cexc.try (Wlk.pathWalkAccumulate rootPath filesAnalyser) :: IO (Either Serr.IOError RType)
  case mbRez of
    Left exception -> do
      if Serr.isDoesNotExistErrorType . Serr.ioeGetErrorType $ exception then do
        pure ()
      else
        putStrLn $ "@[loadFolderTree] err: " <> show exception
      pure Seq.empty
    Right rez -> pure rez


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
