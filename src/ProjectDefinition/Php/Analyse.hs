module ProjectDefinition.Php.Analyse where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bsl
import Data.Binary.Put (runPut, putInt32be)
import Data.Either (lefts, rights, partitionEithers)
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import System.FilePath.Posix ((</>))

import Hasql.Pool (Pool)

import TreeSitter.Node (TSPoint(..))
import Cannelle.TreeSitter.Types (SegmentPos)
import Cannelle.PHP.Parse (tsParsePhp)
import Cannelle.PHP.AST (PhpContext (..), PhpAction (..)
        , PhpStatement (..), DanglingClosure (..), PhpExpression (..)
        , CallerSpec (..), MemberAccessMode (..), ScopeMode (..), IncludeMode (..)
        , VariableSpec (..), UnaryOps (..), BinaryOps (..), UpdateOp (..)
        , LiteralValue (..), StringDetails (..), EncapsedMode (..)
        , MemberModifier (..), Attribute (..), AttributeGroup (..), AttributeList (..)
        , ClassMemberDecl (..), UseList (..), MethodImplementation (..)
        , TypeDecl (..), QualifiedName (..))
import Cannelle.PHP.Print (printPhpContext)
import qualified Cannelle.PHP.Serialize as PhS
import qualified Cannelle.Common.Serialize as CnC

import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs

import qualified DB.FileOps as Do
import Utils (seqPartitionEithers)


processDir :: Text -> FilePath -> Pool -> IO ()
processDir projectName rootPath dbPool = do
  eiPathFiles <- Fs.loadFolderTree rootPath
  case eiPathFiles of
    Left err -> putStrLn $ "@[php.processDir] error loading folder tree: " <> rootPath <> " - " <> err
    Right pathFiles -> do
      eiProject <- Do.getProject dbPool projectName "php"
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
  fRegistries <- mapM (registerFile dbPool folderID fullPath) (filter Fs.isPhpExtItem files)
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
            Just (format, ast) -> do
              putStrLn $ "@[registerFile] getAst format: " <> show format <> " ast: " <> show (Bs.length ast)
              pure $ Right ()
            Nothing -> do
              let
                fullFilePath = fullDirPath </> fileItemPath
              startTime <- getCurrentTime
              parseRez <- tsParsePhp False fullFilePath
              endTime <- getCurrentTime
              let
                duration = diffUTCTime endTime startTime
              putStrLn $ "@[registerFile] parse time: " <> show duration <> ", rez: " <> show parseRez
              case parseRez of
                Left err -> do
                  rezB <- Do.addError dbPool fileID (pack $ show err) (Just duration)
                  case rezB of
                    Left dbErr -> pure . Left $ show dbErr
                    Right _ -> pure . Left $ show err
                Right phpContext -> do
                  compactConstants <- CnC.compactText fullFilePath phpContext.contentDemands
                  let
                    bsAst = PhS.convertAST phpContext.logic compactConstants
                    bsConstants = CnC.convertConstants compactConstants
                  rezC <- Do.addAST dbPool fileID "php" bsAst
                  rezD <- Do.addConstants dbPool fileID bsConstants
                  case (rezC, rezD) of
                    (Left dbErr, _) -> pure . Left $ "addAST err: " <> show dbErr
                    (_, Left dbErr) -> pure . Left $ "addConstants err: " <> show dbErr
                    (Right _, Right _) -> pure $ Right ()
