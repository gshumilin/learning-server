{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Environment where

import Data.Aeson.Types (FromJSON)
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
  deriving (Show, Generic, FromJSON)

data DbConnectInfo = DbConnectInfo
  { dbConnectHost :: String,
    dbConnectPort :: Word16,
    dbConnectUser :: String,
    dbConnectPassword :: String,
    dbConnectDatabase :: String
  }
  deriving (Generic, Show, FromJSON)
