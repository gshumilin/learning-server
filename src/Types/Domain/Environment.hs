module Types.Domain.Environment where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, Value (..), parseJSON, (.:))
import Data.Word (Word16)
import Database.PostgreSQL.Simple (Connection)
import Types.Domain.Log (LogInfo (..))

data Environment = Environment
  { dbConnection :: Connection,
    logInfo :: LogInfo
  }

data Config = Config
  { serverPort :: Int,
    dbConnectInfo :: DbConnectInfo,
    logInfo :: LogInfo
  }

instance FromJSON Config where
  parseJSON (Object inputJSON) = do
    serverPort <- inputJSON .: "serverPort"
    dbConnectInfo <- inputJSON .: "dbConnectInfo"
    logInfo <- inputJSON .: "logInfo"
    pure Config {..}
  parseJSON _ = mzero

data DbConnectInfo = DbConnectInfo
  { dbConnectHost :: String,
    dbConnectPort :: Word16,
    dbConnectUser :: String,
    dbConnectPassword :: String,
    dbConnectDatabase :: String
  }

instance FromJSON DbConnectInfo where
  parseJSON (Object inputJSON) = do
    dbConnectHost <- inputJSON .: "connectHost"
    dbConnectPort <- inputJSON .: "connectPort"
    dbConnectUser <- inputJSON .: "connectUser"
    dbConnectPassword <- inputJSON .: "connectPassword"
    dbConnectDatabase <- inputJSON .: "connectDatabase"
    pure DbConnectInfo {..}
  parseJSON _ = mzero
