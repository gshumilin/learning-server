module Types.Domain.Environment where

import Types.Domain.Log (LogLvl(..))
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..))
import Data.Aeson.Types

data Environment = Environment 
    {   dbConnection :: Connection,
        logLvl :: LogLvl
    }

data Config = Config
    {   serverPort :: Int,
        dbConnectInfo :: ConnectInfo,
        logLvl :: LogLvl 
    }

instance FromJSON Config where
    parseJSON (Object inputJSON) = do
        serverPort <- inputJSON .: "serverPort"
        dbConnectInfo <- inputJSON .: "dbConnectInfo"
        logLvl <- inputJSON .: "logLvl"
        return Config {..}

instance FromJSON ConnectInfo where
    parseJSON (Object inputJSON) = do
        connectHost <- inputJSON .: "connectHost"	 
        connectPort <- inputJSON .: "connectPort"	 
        connectUser <- inputJSON .: "connectUser"	 
        connectPassword <- inputJSON .: "connectPassword"	 
        connectDatabase <- inputJSON .: "connectDatabase"	 
        return ConnectInfo {..}