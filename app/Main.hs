module Main where

import Control.Monad.Reader (runReaderT)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (ConnectInfo (..), connect)
import Log (addLog)
import Network.Wai.Handler.Warp (run)
import Routing (application)
import Types.Domain.Environment (Config (..), DbConnectInfo (..), Environment (..))
import Types.Domain.Log (LogLvl (..))

main :: IO ()
main = do
  conf <- getConfig
  env <- buildEnvironment conf
  let port = serverPort conf
  let app req respond = runReaderT (application req respond) env
  runReaderT (addLog RELEASE "_____ Server started _____") env
  runReaderT (addLog DEBUG ("port = " ++ show port ++ "\n")) env
  run port app

buildEnvironment :: Config -> IO Environment
buildEnvironment Config {..} = do
  let DbConnectInfo {..} = dbConnectInfo
  let connectInfo = ConnectInfo dbConnectHost dbConnectPort dbConnectUser dbConnectPassword dbConnectDatabase
  conn <- connect connectInfo
  pure $ Environment conn logInfo

getConfig :: IO Config
getConfig = do
  rawJSON <- BS.readFile "config.json"
  case decodeStrict rawJSON of
    Nothing -> error "Config cannot be read. Invalid JSON"
    Just conf -> pure conf
