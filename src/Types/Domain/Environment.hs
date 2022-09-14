{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Environment where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, Value (..), parseJSON, (.:))
import Data.Word (Word16)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import System.IO (Handle)
import Types.Domain.Log (LogDescType (..), LogLvl (..))

data Environment = Environment
  { dbConnection :: Connection,
    logLvl :: LogLvl,
    logDesc :: Handle
  }

data Config = Config
  { serverPort :: Int,
    dbConnectInfo :: DbConnectInfo,
    logLvl :: LogLvl,
    logDescType :: LogDescType
  }
  deriving (Show, Generic)

instance FromJSON Config

data DbConnectInfo = DbConnectInfo
  { dbConnectHost :: String,
    dbConnectPort :: Word16,
    dbConnectUser :: String,
    dbConnectPassword :: String,
    dbConnectDatabase :: String
  }
  deriving (Show)

instance FromJSON DbConnectInfo where
  parseJSON (Object inputJSON) = do
    dbConnectHost <- inputJSON .: "connectHost"
    dbConnectPort <- inputJSON .: "connectPort"
    dbConnectUser <- inputJSON .: "connectUser"
    dbConnectPassword <- inputJSON .: "connectPassword"
    dbConnectDatabase <- inputJSON .: "connectDatabase"
    pure DbConnectInfo {..}
  parseJSON _ = mzero
