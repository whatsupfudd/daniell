module DB.Connect where

import Control.Exception (bracket)
import Control.Monad.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (DiffTime)

import GHC.Word (Word16)

import qualified Hasql.Connection as DbConn
import Hasql.Pool (Pool, acquire, release)


data PgDbConfig = PgDbConfig {
  port :: Word16
  , host :: ByteString
  , user :: ByteString
  , passwd :: ByteString
  , dbase :: ByteString
  , poolSize :: Int
  , acqTimeout :: DiffTime
  , poolTimeOut :: DiffTime
  , poolIdleTime :: DiffTime
  }
  deriving (Show)


defaultPgDbConf = PgDbConfig {
  port = 5432
  , host = "test"
  , user = "test"
  , passwd = "test"
  , dbase = "test"
  , poolSize = 5
  , acqTimeout = 5
  , poolTimeOut = 60
  , poolIdleTime = 300
  }


startPg :: PgDbConfig -> ContT r IO Pool
startPg dbC =
  let
    dbSettings = DbConn.settings dbC.host dbC.port dbC.user dbC.passwd dbC.dbase
  in do
  liftIO . putStrLn $ "@[startPg] user: " <> show dbC.user <> " db: " <> show dbC.dbase <> "."
  ContT $ bracket (acquire dbC.poolSize dbC.acqTimeout dbC.poolTimeOut dbC.poolIdleTime dbSettings) release
