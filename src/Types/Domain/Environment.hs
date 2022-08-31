module Types.Domain.Environment where

import Types.Domain.Log
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..))
import Data.Aeson.Types

data Environment = Environment 
    {   dbConnection :: Connection,
        logInfo :: LogInfo
    }

data Config = Config
    {   serverPort :: Int,
        dbConnectInfo :: ConnectInfo,
        logInfo :: LogInfo 
    }

instance FromJSON Config where
    parseJSON (Object inputJSON) = do
        serverPort <- inputJSON .: "serverPort"
        dbConnectInfo <- inputJSON .: "dbConnectInfo"
        logInfo <- inputJSON .: "logInfo"
        pure Config {..}

instance FromJSON ConnectInfo where
    parseJSON (Object inputJSON) = do
        connectHost <- inputJSON .: "connectHost"
        connectPort <- inputJSON .: "connectPort"
        connectUser <- inputJSON .: "connectUser"
        connectPassword <- inputJSON .: "connectPassword"
        connectDatabase <- inputJSON .: "connectDatabase"
        pure ConnectInfo {..}