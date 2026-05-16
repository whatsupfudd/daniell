{-# LANGUAGE QuasiQuotes #-}

module DB.FileStmt where

import qualified Data.ByteString as Bs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.UUID (UUID)

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH


locateProject :: Text -> Session (Maybe (Int32, UUID))
locateProject projectName =
  statement projectName [TH.maybeStatement|
    select uid::int4, eid::uuid from project where label = $1::text
  |]


insertProject :: Text -> Text -> Session (Int32, UUID)
insertProject projectName kind =
  statement (projectName, kind) [TH.singletonStatement|
    insert into project (label, kind) values ($1::text, $2::text) returning uid::int4, eid::uuid
  |]


locateFolder :: Int32 -> Text -> Session (Maybe Int32)
locateFolder projectID path =
  statement (projectID, path) [TH.maybeStatement|
    select uid::int4 from ing.folder where project_fk = $1::int4 and path = $2::text
  |]


insertFolder :: Int32 -> Text -> Session Int32
insertFolder projectID path =
  statement (projectID, path) [TH.singletonStatement|
    insert into ing.folder (project_fk, path) values ($1::int4, $2::text) returning uid::int4
  |]


locateFile :: Int32 -> Text -> Session (Maybe Int32)
locateFile folderID path =
  statement (folderID, path) [TH.maybeStatement|
    select uid::int4 from ing.file where folder_fk = $1::int4 and path = $2::text
  |]


insertFile :: Int32 -> Text -> Session Int32
insertFile folderID path =
  statement (folderID, path) [TH.singletonStatement|
    insert into ing.File (folder_fk, path) values ($1::int4, $2::text) returning uid::int4
  |]


selectAST :: Int32 -> Session (Maybe (Text, Bs.ByteString))
selectAST fileID =
  statement fileID [TH.maybeStatement|
    select format::text, value::bytea from ing.AST where file_fk = $1::int4
  |]

insertAST :: Int32 -> Text -> Bs.ByteString -> Session ()
insertAST fileID format ast =
  statement (fileID, format, ast) [TH.resultlessStatement|
    insert into ing.AST (file_fk, format, value) values ($1::int4, $2::text, $3::bytea)
  |]

insertConstants :: Int32 -> Bs.ByteString -> Session ()
insertConstants fileID constants =
  statement (fileID, constants) [TH.resultlessStatement|
    insert into ing.Constant (file_fk, value) values ($1::int4, $2::bytea)
  |]


insertError :: Int32 -> Text -> Maybe NominalDiffTime -> Session ()
insertError fileID message mbProcTime =
  statement (fileID, message, realToFrac <$> mbProcTime) [TH.resultlessStatement|
    insert into ing.Error (file_fk, message, procTime) values ($1::int4, $2::text, $3::float4?)
  |]
