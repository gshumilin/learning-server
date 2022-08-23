module Types.Domain.Environment where

import Database.PostgreSQL.Simple (Connection, ConnectInfo(..))
import Data.Aeson.Types

data Environment = Environment 
    {   config :: Config,
        dbConnection :: Connection
    }

data Config = Config
    {   dbConnectInfo :: ConnectInfo        
    }

instance FromJSON Config where
    parseJSON (Object inputJSON) = do
        dbConnectInfo <- inputJSON .: "dbConnectInfo"
        return Config {..}

instance FromJSON ConnectInfo where
    parseJSON (Object inputJSON) = do
        connectHost <- inputJSON .: "connectHost"	 
        connectPort <- inputJSON .: "connectPort"	 
        connectUser <- inputJSON .: "connectUser"	 
        connectPassword <- inputJSON .: "connectPassword"	 
        connectDatabase <- inputJSON .: "connectDatabase"	 
        return ConnectInfo {..}