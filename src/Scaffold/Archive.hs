module Scaffold.Archive where

import qualified Data.ByteString.Lazy as Lbs
import Data.Either (lefts, rights)
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Sequence as Seq
import Data.Text (Text)

import System.FilePath (joinPath, splitDirectories, makeRelative, takeFileName, (</>), takeDirectory)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.BZip as BZip

import qualified Scaffold.Types as St
import qualified Cannelle.Templog.Types as Tpl
import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fe
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)


listTarBz2Contents :: FilePath -> IO ()
listTarBz2Contents archivePath = do
    compressedData <- Lbs.readFile archivePath
    let
      decompressedData = BZip.decompress compressedData
      entries = Tar.read decompressedData
      eiFilePaths = getEntries (Right []) entries
    case eiFilePaths of
      Right filePaths -> mapM_ (putStrLn . showEntry) filePaths
      someErrs -> putStrLn $ "Errors: " <> show someErrs

getEntries :: Either [String] [Tar.Entry] -> Tar.Entries Tar.FormatError -> Either [String] [Tar.Entry]
getEntries accum entries =
  case accum of
    Left errs ->
      case entries of
        Tar.Fail err -> Left (errs <> [show err])
        _ -> accum
    Right filePaths ->
      case entries of
        Tar.Done -> Right filePaths
        Tar.Fail err -> Left [show err]
        Tar.Next entry rest ->
          getEntries (Right (filePaths <> [entry])) rest

showEntry :: Tar.Entry -> String
showEntry entry =
  case Tar.entryContent entry of
    Tar.NormalFile content size ->
      "F: " <> Tar.entryPath entry <> " (" <> show size <> ")"
    Tar.Directory ->
      "D: " <> Tar.entryPath entry
    _ -> "other"


loadArchive :: FilePath -> IO (Either String St.ScaffoldBundle)
loadArchive archivePath = do
  compressedData <- Lbs.readFile archivePath
  let
    decompressedData = BZip.decompress compressedData
    entries = Tar.read decompressedData
    eiEntries = getEntries (Right []) entries
  case eiEntries of
    Left errs -> pure . Left $ L.intercalate "\n" errs
    Right tEntries ->
      let
        dirMap = foldl (\accum entry ->
          case entry.entryContent of
            Tar.Directory -> addDirToMap accum $ splitDirectories (Tar.entryPath entry)
            Tar.NormalFile content size ->
              case Fe.itemMaker . takeFileName $ Tar.entryPath entry of
                Nothing -> accum
                Just item ->
                  let
                    pComponents = splitDirectories . takeDirectory $ Tar.entryPath entry
                  in
                  addFileToMap accum pComponents (item, content)
            _ -> accum
          ) Mp.empty tEntries
        fileTree = dirMapToPathNodes Nothing (Mp.elems dirMap)
      in do
      -- putStrLn $ "@[loadArchive] dirMap: " <> show dirMap
      pure . Right $ St.ScaffoldBundle archivePath Nothing Nothing fileTree mempty Tpl.Noop


dirMapToPathNodes :: Maybe FilePath -> [ Fs.DirNode ] -> Seq.Seq Fs.PathNode
dirMapToPathNodes mbPrefix dirNodes =
  case dirNodes of
    [] -> Seq.empty
    aNode : rest ->
      let
        nodePath = maybe aNode.dirPath (</> aNode.dirPath) mbPrefix
        curNode = Seq.singleton (nodePath, aNode.files)
        childrenNodes = dirMapToPathNodes (Just nodePath) (Mp.elems aNode.subTree)
      in
        curNode <> childrenNodes <> dirMapToPathNodes mbPrefix rest


addDirToMap :: Mp.Map FilePath Fs.DirNode -> [FilePath] -> Mp.Map FilePath Fs.DirNode
addDirToMap accum components =
  case components of
    [] -> accum
    aPath : rest ->
      case Mp.lookup aPath accum of
        Nothing ->
          let
            newNode = Fs.DirNode aPath (componentsToTree rest) []
          in
          Mp.insert aPath newNode accum
        Just aDirNode ->
          let
            newSubTree = addDirToMap aDirNode.subTree rest
          in
          Mp.insert aPath (aDirNode { Fs.subTree = newSubTree }) accum


componentsToTree :: [FilePath] -> Mp.Map FilePath Fs.DirNode
componentsToTree =
  foldr (\aPath subTree ->
      Mp.singleton aPath (Fs.DirNode aPath subTree [])
    ) Mp.empty


addFileToMap :: Mp.Map FilePath Fs.DirNode -> [FilePath] -> (Fs.FileItem, Lbs.ByteString) -> Mp.Map FilePath Fs.DirNode
addFileToMap accum components (item, content) =
  let
    contentItem = Fs.ContentFI item content
  in
  case components of
    [] ->
      case Mp.lookup "<root>" accum of
        Nothing ->
          Mp.insert "<root>" (Fs.DirNode "<root>" Mp.empty [contentItem]) accum
        Just aNode ->
          let
            newNode = aNode { Fs.files = contentItem : aNode.files }
          in
          Mp.insert "<root>" newNode accum 
    aPath : rest ->
      case Mp.lookup aPath accum of
        Nothing ->
          let
            newNode = Fs.DirNode aPath (componentsToTree rest) [contentItem]
          in
          Mp.insert aPath newNode accum
        Just aDirNode ->
          let
            updNode = case rest of
              [] -> aDirNode { Fs.files = contentItem : aDirNode.files }
              _ ->
                let
                  newSubTree = addFileToMap aDirNode.subTree rest (item, content)
                in
                aDirNode { Fs.subTree = newSubTree }
          in
          Mp.insert aPath updNode accum