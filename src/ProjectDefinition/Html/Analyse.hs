module ProjectDefinition.Html.Analyse where

import qualified Data.ByteString as Bs
import Data.Either (partitionEithers)
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as Tio
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.FilePath.Posix ((</>))

import Hasql.Pool (Pool)

import Text.HTML.Parser (parseTokens)
import Text.HTML.Tree (tokensToForest)

import qualified FileSystem.Explore as Fs
import qualified FileSystem.Types as Fs

import qualified DB.FileOps as Do
import Utils (seqPartitionEithers)
import qualified ProjectDefinition.Html.Parser as Pr


processDir :: Text -> FilePath -> Pool -> IO ()
processDir projectName rootPath dbPool = do
  eiPathFiles <- Fs.loadFolderTree rootPath
  case eiPathFiles of
    Left err -> putStrLn $ "@[php.processDir] error loading folder tree: " <> rootPath <> " - " <> err
    Right pathFiles -> do
      eiProject <- Do.getProject dbPool projectName
      case eiProject of
        Left err -> putStrLn $ "@[php.processDir] getProject err: " <> err
        Right projectID -> do
          eiRezA <- mapM (\aPathFile ->
              let
                dirPath = fst aPathFile
              in do
              eiRezB <- Do.getFolder dbPool projectID dirPath
              case eiRezB of
                Left err -> pure $ Left err
                Right folderID -> pure $ Right (pack dirPath, folderID)
              ) pathFiles
          case seqPartitionEithers eiRezA of
            ([], folderIDs) ->
              let
                folderIDMap = Mp.fromList folderIDs
              in
              mapM_ (processFilesInDir dbPool folderIDMap rootPath) pathFiles
            (errs, _) -> do
              putStrLn $ "@[processDir] processFilesInDir errs: " <> show errs
              pure ()


processFilesInDir :: Pool -> Mp.Map Text Int32 -> FilePath -> Fs.PathNode -> IO ()
processFilesInDir dbPool folderIDMap rootPath (dirPath, files) =
  let
    folderID = fromMaybe 0 $ Mp.lookup (pack dirPath) folderIDMap
    fullPath = rootPath </> dirPath
  in do
  putStrLn $ "@[processFilesInDir] folderID: " <> show folderID <> " fullPath: " <> fullPath
  fRegistries <- mapM (registerFile dbPool folderID fullPath) (filter Fs.isHtmlExtItem files)
  case partitionEithers fRegistries of
    (errs, _) -> do
      putStrLn $ "@[processFilesInDir] registerFile errs: " <> show errs
      pure ()
  pure ()


registerFile :: Pool -> Int32 -> FilePath -> Fs.ExtFileItem -> IO (Either String ())
registerFile dbPool folderID fullDirPath fileItem = 
  let
    fileItemPath = Fs.getExtFileItemPath fileItem
  in do
  putStrLn $ "@[registerFile] folderID: " <> show folderID <> " file: " <> fileItemPath
  rezA <- Do.getFile dbPool folderID fileItemPath
  case rezA of
    Left err -> do
      putStrLn $ "@[registerFile] getFile err: " <> err
      pure $ Left err
    Right fileID -> do
      putStrLn $ "@[registerFile] getFile fileID: " <> show fileID
      rezA <- Do.getAst dbPool fileID
      case rezA of
        Left err -> do
          putStrLn $ "@[registerFile] getAst err: " <> err
          pure $ Left err
        Right mbAst ->
          case mbAst of
            Just ast -> do
              putStrLn $ "@[registerFile] getAst ast: " <> show (Bs.length ast)
              pure $ Right ()
            Nothing -> do
              let
                fullFilePath = fullDirPath </> fileItemPath
              startTime <- getCurrentTime
              parseRez <- parseHtml fullFilePath
              endTime <- getCurrentTime
              pure $ Left "unimplemented"


parseHtml :: FilePath -> IO (Either String Bs.ByteString)
parseHtml filePath = do
  inFile <- Tio.readFile filePath
  let
    tokens = parseTokens inFile
    htmlDocument = Pr.htmlDocumentFromTokens tokens
    (textPool, htmlDocumentC) = Pr.compactHtmlDocument htmlDocument
    orderedTextPool = Pr.orderedTextPool textPool
  putStrLn $ "@[parseHtml] orderedTextPool: " <> show orderedTextPool <> "\nhtmlDocumentC: " <> show htmlDocumentC
  pure $ Right Bs.empty
