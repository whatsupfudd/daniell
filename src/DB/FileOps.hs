
module DB.FileOps where

import qualified Data.ByteString as Bs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)

import qualified DB.FileStmt as Fs


getProject :: Pool -> Text -> IO (Either String Int32)
getProject dbPool projectName = do
  rezA <- use dbPool $ Fs.locateProject projectName
  case rezA of
    Left err -> pure . Left $ "@[getProject] locateProject err: " <> show err
    Right mbUid -> case mbUid of
      Just (uid, _) -> pure $ Right uid
      Nothing -> do
        rezB <- use dbPool $ Fs.insertProject projectName
        case rezB of
          Left err -> pure . Left $ "@[getProject] insertProject err: " <> show err
          Right (uid, _) -> pure $ Right uid


getFolder :: Pool -> Int32 -> FilePath -> IO (Either String Int32)
getFolder dbPool projectID filePath =
  let
    path = pack filePath
  in do
  rezA <- use dbPool $ Fs.locateFolder projectID path
  case rezA of
    Left err -> pure . Left $ "@[getFolder] locateFolder err: " <> show err
    Right mbUid -> case mbUid of
      Just uid -> pure $ Right uid
      Nothing -> do
        rezB <- use dbPool $ Fs.insertFolder projectID path
        case rezB of
          Left err -> pure . Left $ "@[getFolder] insertFolder err: " <> show err
          Right uid -> pure $ Right uid


getFile :: Pool -> Int32 -> String -> IO (Either String Int32)
getFile dbPool folderID filePath =
  let
    path = pack filePath
  in do
  rezA <- use dbPool $ Fs.locateFile folderID path
  case rezA of
    Left err -> pure . Left $ "@[getFile] locateFile err: " <> show err
    Right mbUid -> case mbUid of
      Just uid -> pure $ Right uid
      Nothing -> do
        rezB <- use dbPool $ Fs.insertFile folderID path
        case rezB of
          Left err -> pure . Left $ "@[getFile] insertFile err: " <> show err
          Right uid -> pure $ Right uid


addError :: Pool -> Int32 -> Text -> Maybe NominalDiffTime -> IO (Either String ())
addError dbPool fileID message procTime = do
  rezA <- use dbPool $ Fs.insertError fileID message procTime
  case rezA of
    Left err -> pure . Left $ "@[addError] insertError err: " <> show err
    Right _ -> pure $ Right ()


getAst :: Pool -> Int32 -> IO (Either String (Maybe Bs.ByteString))
getAst dbPool fileID = do
  rezA <- use dbPool $ Fs.selectAST fileID
  case rezA of
    Left err -> pure . Left $ "@[getAst] selectAST err: " <> show err
    Right mbAst -> pure $ Right mbAst


addAST :: Pool -> Int32 -> Bs.ByteString -> IO (Either String ())
addAST dbPool fileID ast = do
  rezA <- use dbPool $ Fs.insertAST fileID ast
  case rezA of
    Left err -> pure . Left $ "@[addAST] insertAST err: " <> show err
    Right _ -> pure $ Right ()


addConstants :: Pool -> Int32 -> Bs.ByteString -> IO (Either String ())
addConstants dbPool fileID constants = do
  rezA <- use dbPool $ Fs.insertConstants fileID constants
  case rezA of
    Left err -> pure . Left $ "@[addConstants] insertConstants err: " <> show err
    Right _ -> pure $ Right ()
