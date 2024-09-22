{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
module FileSystem.Explore where

import Control.Monad (forM_)
import qualified Control.Exception as Cexc
import qualified Data.Char as DC
import Data.List (isSuffixOf)
import qualified Data.Map as Mp
import Data.Text (Text)
-- import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Vector as Vc
import qualified Data.Sequence as Seq
import qualified System.Directory.PathWalk as Wlk
import System.FilePath (joinPath, splitDirectories, makeRelative)
import qualified System.IO.Error as Serr

import Options.Runtime (RunOptions (..))
import FileSystem.Types


loadFolderTree :: FilePath -> IO (Either String PathFiles)
loadFolderTree rootPath = do
  -- DBG: putStrLn "@[loadFolderTree] starting."
  eiRez <- Cexc.try (Wlk.pathWalkAccumulate rootPath (filesAnalyser $ 1 + length rootPath)) :: IO (Either Serr.IOError PathFiles)
  case eiRez of
    Left exception -> pure . Left $ "@[loadFolderTree] err: " <> show exception
    Right rez -> pure $ Right rez


filesAnalyser :: Int -> FilePath -> [FilePath] -> [[Char]] -> IO PathFiles
filesAnalyser prefixLength root dirs files =
  if ".git" `elem` splitDirectories root then
    pure Seq.empty
  else
    let
      items = foldl (\accum file ->
                      case itemMaker file of
                        Nothing -> accum
                        Just anItem -> accum <> [anItem]
                      ) [] files
    in
    if length items > 0 then
      pure $ Seq.singleton (drop prefixLength root, items)
    else
      pure Seq.empty


itemMaker :: FilePath -> Maybe FileItem
itemMaker fileName
  | ".html" `isSuffixOf` fileName = Just $ KnownFile Html fileName
  | ".yaml" `isSuffixOf` fileName = Just $ KnownFile Yaml fileName
  | ".toml" `isSuffixOf` fileName = Just $ KnownFile Toml fileName
  | ".json" `isSuffixOf` fileName = Just $ KnownFile Json fileName
  | ".dant" `isSuffixOf` fileName = Just $ KnownFile DanTmpl fileName
  | ".hs" `isSuffixOf` fileName = Just $ KnownFile Haskell fileName
  | ".elm" `isSuffixOf` fileName = Just $ KnownFile Elm fileName
  | ".ts" `isSuffixOf` fileName = Just $ KnownFile Typescript fileName
  | ".js" `isSuffixOf` fileName = Just $ KnownFile Javascript fileName
  | ".tsx" `isSuffixOf` fileName = Just $ KnownFile TsReact fileName
  | ".jsx" `isSuffixOf` fileName = Just $ KnownFile JsReact fileName
  | ".md" `isSuffixOf` fileName = Just $ KnownFile Markdown fileName
  | ".org" `isSuffixOf` fileName = Just $ KnownFile EmacsOrg fileName
  | ".css" `isSuffixOf` fileName = Just $ KnownFile Css fileName
  | ".adoc" `isSuffixOf` fileName = Just $ KnownFile AsciiDoc fileName
  | ".pandoc" `isSuffixOf` fileName = Just $ KnownFile Pandoc fileName
  | ".rss" `isSuffixOf` fileName = Just $ KnownFile Rss fileName
  | ".xml" `isSuffixOf` fileName = Just $ KnownFile Xml fileName
  | ".txt" `isSuffixOf` fileName = Just $ KnownFile TxtTempl fileName
  | otherwise = Just $ MiscFile fileName


buildDirTree :: RunOptions -> IO (Either String DirTreeMap)
buildDirTree rtOpts = do
  eiFileList <- loadFolderTree rtOpts.baseDir
  case eiFileList of
    Left errMsg -> pure . Left $ errMsg
    Right fileLists ->
      let
        prefixLength = length rtOpts.baseDir
        pathList = Seq.foldlWithIndex (massgeDirName rtOpts.baseDir) (Mp.empty :: DirTreeMap) fileLists
      -- TODO: construct a proper site definition from the file list.
      in do
      {-- DBG:
      putStrLn $ "[buildDirTree] # of dir lists: " <> show (length pathList)
      putStrLn $ "[buildDirTree] # of dir lists: " <> (Mp.foldl (\accum node -> accum <> "\n" <> (showNode "" node)) "" pathList) -- Mp.keys
      --}
      pure . Right $ pathList

  where
  massgeDirName baseDir dMap index (aPath, fileList) =
    let
      relDirs = splitDirectories $ makeRelative baseDir aPath -- drop prefixLength aPath
    in
    addTreeNode dMap relDirs fileList

  addTreeNode dMap dirList fileItems =
    case dirList of
      -- Protect against a weird case:
      [] -> dMap
      hDir : tDirs ->
        case Mp.lookup hDir dMap of
          Nothing ->
            case tDirs of
              [] ->
                let
                  newNode = DirNode hDir Mp.empty fileItems
                in
                Mp.insert hDir newNode dMap
              _ ->
                let
                  subTree = addTreeNode Mp.empty tDirs fileItems
                  newNode = DirNode hDir subTree []
                in
                Mp.insert hDir newNode dMap
          Just aNode ->
            case tDirs of
              [] ->
                let
                  newNode = aNode { files = aNode.files <> fileItems }
                in
                Mp.insert hDir newNode dMap
              _ ->
                let
                  newSubTree = addTreeNode aNode.subTree tDirs fileItems
                  newNode = aNode { subTree = newSubTree }
                in
                Mp.insert hDir newNode dMap


showNode :: String -> DirNode -> String
showNode tab node =
  let
    mainPath = tab <> "| " <> node.dirPath <> " : " <> foldl (\accum f -> if accum == "" then show f else accum <> ", " <> show f) "" node.files
  in
  if Mp.size node.subTree == 0 then
    mainPath
  else
    mainPath <> "\n" <> tab <> "|-->" <> Mp.foldl (\accum st -> accum <> "\n" <> showNode (tab <> "  ") st) "" node.subTree

