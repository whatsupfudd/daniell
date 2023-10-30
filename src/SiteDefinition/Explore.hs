module SiteDefinition.Explore where

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

import Options.RunOptions (RunOptions (..))
import SiteDefinition.Types (SiteDefinition (..), TmpFileDef (..))


data FileItem =
  MarkupFI FilePath
  | HtmlFI FilePath
  | YamlFI FilePath
  | TomlFI FilePath
  | JsonFI FilePath
  deriving (Show)

type RType = Seq.Seq (FilePath, [FileItem])

type DirTreeMap = Mp.Map FilePath DirNode

data DirNode = DirNode {
    dirPath :: FilePath
    , subTree :: DirTreeMap
    , files :: [FileItem]
  }

showNode :: String -> DirNode -> String
showNode tab node =
  let
    mainPath = tab <> "| " <> node.dirPath <> " : " <> (foldl (\accum f -> if accum == "" then show f else accum <> ", " <> show f) "" node.files)
  in
  if Mp.size node.subTree == 0 then
    mainPath
  else
    mainPath <> "\n" <> tab <> "|-->" <> (Mp.foldl (\accum st -> accum <> "\n" <> (showNode (tab <> "  ") st)) "" node.subTree)


buildGenDef :: RunOptions -> IO (Either Text (SiteDefinition TmpFileDef))
buildGenDef rtOpts = do
  -- descent into the relevant folders for the generation of the SiteDefinition
  fileLists <- loadFolderTree rtOpts.baseDir
  let
    prefixLength = length rtOpts.baseDir
    pathList = Seq.foldlWithIndex (massgeDirName rtOpts.baseDir) (Mp.empty :: DirTreeMap) fileLists
  -- TODO: construct a proper site definition from the file list.
  {-- DBG:
  putStrLn $ "[buildGenDef] # of dir lists: " <> show (length pathList)
  putStrLn $ "[buildGenDef] # of dir lists: " <> (Mp.foldl (\accum node -> accum <> "\n" <> (showNode "" node)) "" pathList) -- Mp.keys
  --}
  pure $ Right (SiteDefinition { baseDir = rtOpts.baseDir })
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


getContentPages :: SiteDefinition TmpFileDef -> [ FilePath ]
getContentPages siteDef =
  -- find the content pages in the SiteDefinition
  -- TODO:
  []


loadFolderTree :: FilePath -> IO RType
loadFolderTree rootPath = do
  -- DBG: putStrLn "@[loadFolderTree] starting."
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
