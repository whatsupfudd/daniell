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
    select uid::int4, eid::uuid from project where label = $1::text and kind = 'php'
  |]


insertProject :: Text -> Session (Int32, UUID)
insertProject projectName =
  statement projectName [TH.singletonStatement|
    insert into project (label, kind) values ($1::text, 'php') returning uid::int4, eid::uuid
  |]


locateFolder :: Int32 -> Text -> Session (Maybe Int32)
locateFolder projectID path =
  statement (projectID, path) [TH.maybeStatement|
    select uid::int4 from folder where project_fk = $1::int4 and path = $2::text
  |]


insertFolder :: Int32 -> Text -> Session Int32
insertFolder projectID path =
  statement (projectID, path) [TH.singletonStatement|
    insert into folder (project_fk, path) values ($1::int4, $2::text) returning uid::int4
  |]


locateFile :: Int32 -> Text -> Session (Maybe Int32)
locateFile folderID path =
  statement (folderID, path) [TH.maybeStatement|
    select uid::int4 from File where folder_fk = $1::int4 and path = $2::text
  |]


insertFile :: Int32 -> Text -> Session Int32
insertFile folderID path =
  statement (folderID, path) [TH.singletonStatement|
    insert into File (folder_fk, path) values ($1::int4, $2::text) returning uid::int4
  |]


selectAST :: Int32 -> Session (Maybe Bs.ByteString)
selectAST fileID =
  statement fileID [TH.maybeStatement|
    select value::bytea from AST where file_fk = $1::int4
  |]

insertAST :: Int32 -> Bs.ByteString -> Session ()
insertAST fileID ast =
  statement (fileID, ast) [TH.resultlessStatement|
    insert into AST (file_fk, value) values ($1::int4, $2::bytea)
  |]

insertConstants :: Int32 -> Bs.ByteString -> Session ()
insertConstants fileID constants =
  statement (fileID, constants) [TH.resultlessStatement|
    insert into Constant (file_fk, value) values ($1::int4, $2::bytea)
  |]


insertError :: Int32 -> Text -> Maybe NominalDiffTime -> Session ()
insertError fileID message mbProcTime =
  statement (fileID, message, realToFrac <$> mbProcTime) [TH.resultlessStatement|
    insert into Error (file_fk, message, procTime) values ($1::int4, $2::text, $3::float4?)
  |]
