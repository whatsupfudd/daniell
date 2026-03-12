module ProjectDefinition.Html.Analyse where

import qualified Data.ByteString as Bs
import Data.Either (partitionEithers)
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as Tio
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector as V
import System.FilePath.Posix ((</>))

import Hasql.Pool (Pool)

import Text.HTML.Parser (parseTokens)
import Text.HTML.Tree (tokensToForest)

import qualified FileSystem.Explore as Fs
import qualified FileSystem.Types as Fs

import qualified DB.FileOps as Do
import Utils (seqPartitionEithers)

import qualified Cannelle.Html.Parser as Pr
import qualified Cannelle.Html.Serialize as Se
import qualified Cannelle.React.Parse as Rp
import qualified Cannelle.React.Serialize as Rs
import Cannelle.React.AST (ReactContext(..))
import qualified Cannelle.Common.Serialize as CnS


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
  let
    targetFiles = filter (\aFile -> Fs.isHtmlExtItem aFile || Fs.isJsExtItem aFile) files
  fRegistries <- mapM (\aFile ->
      registerFile dbPool folderID (fromJust $ Fs.getExtFileItemKind aFile) fullPath (Fs.getExtFileItemPath aFile)
    ) targetFiles
  case partitionEithers fRegistries of
    (errs, _) -> do
      putStrLn $ "@[processFilesInDir] registerFile errs: " <> show errs
      pure ()
  pure ()


registerFile :: Pool -> Int32 -> Fs.FileKind -> FilePath -> FilePath -> IO (Either String ())
registerFile dbPool folderID fileKind fullDirPath fileItemPath = do
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
            Just (format, ast) -> do
              putStrLn $ "@[registerFile] getAst format: " <> show format <> " ast: " <> show (Bs.length ast)
              pure $ Right ()
            Nothing -> do
              let
                fullFilePath = fullDirPath </> fileItemPath
              startTime <- getCurrentTime
              parseRez <- parseFile fileKind fullFilePath
              endTime <- getCurrentTime
              case parseRez of
                Left err -> do
                  putStrLn $ "@[registerFile] parseFile err: " <> err
                  pure $ Left err
                Right (serPool, serDoc) -> do
                  rezC <- Do.addAST dbPool fileID (Fs.fileKindToStr fileKind) serDoc
                  rezD <- Do.addConstants dbPool fileID serPool
                  case (rezC, rezD) of
                    (Left dbErr, _) -> pure . Left $ "addAST err: " <> show dbErr
                    (_, Left dbErr) -> pure . Left $ "addConstants err: " <> show dbErr
                    (Right _, Right _) -> pure $ Right ()


parseFile :: Fs.FileKind -> FilePath -> IO (Either String (Bs.ByteString, Bs.ByteString))
parseFile fileKind filePath = do
  case fileKind of
    Fs.Html -> parseHtml filePath
    Fs.Javascript -> parseJs filePath
    _ -> pure $ Left $ "Unsupported file kind: " <> show fileKind


parseHtml :: FilePath -> IO (Either String (Bs.ByteString, Bs.ByteString))
parseHtml filePath = do
  inFile <- Tio.readFile filePath
  let
    tokens = parseTokens inFile
    htmlDocument = Pr.htmlDocumentFromTokens tokens
    (textPool, htmlDocumentC) = Pr.compactHtmlDocument htmlDocument
    orderedTextPool = Pr.orderedTextPool textPool
    eiSerPool = Se.serializeCompactText orderedTextPool
    serDoc = Se.serializeHtmlDocumentC htmlDocumentC
    eiDeserDoc = Se.deserializeHtmlDocumentC serDoc
  case eiSerPool of
    Left err -> pure $ Left err
    Right serPool ->
      case eiDeserDoc of
        Left err -> pure $ Left err
        Right deserDoc -> do
          putStrLn $ "@[parseHtml] orderedTextPool: " <> show (length orderedTextPool) <> " entries, " <> show (Bs.length serPool) <> " bytes."
          putStrLn $ "@[parseHtml] serDoc: " <> show (Bs.length serDoc) <> " bytes."
          pure $ Right (serPool, serDoc)


parseJs :: FilePath -> IO (Either String (Bs.ByteString, Bs.ByteString))
parseJs filePath = do
  eiReactCtxt <- Rp.tsParseReact False filePath
  case eiReactCtxt of
    Left err -> pure $ Left $ show err
    Right reactCtxt -> do
      textMap <- CnS.compactText filePath reactCtxt.contentDemands
      let
        serCtePool = CnS.convertConstants textMap
        serDoc = Rs.serializeTsxAst Rs.defaultSerializeOptions $ V.toList reactCtxt.tlElements
      pure $ Right (serCtePool, serDoc)
