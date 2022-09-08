module Main where

import Control.Exception (IOException, catch)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (ConnectInfo (..), connect, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), runMigration)
import Log (addLog)
import Network.Wai.Handler.Warp (run)
import Routing (application)
import System.Environment (getArgs)
import Types.Domain.Environment (Config (..), DbConnectInfo (..), Environment (..))
import Types.Domain.Log (LogLvl (..))

main :: IO ()
main = do
  mbConf <- getConfig
  case mbConf of
    Nothing -> putStrLn "Config wasn't parsed! Server wasn't started"
    Just conf -> do
      env <- buildEnvironment conf
      let port = serverPort conf
      let app req respond = runReaderT (application req respond) env
      args <- getArgs
      runReaderT (mapM_ argProcessing args) env
      runReaderT (addLog RELEASE "_____ Server started _____") env
      runReaderT (addLog DEBUG ("port = " ++ show port ++ "\n")) env
      run port app

buildEnvironment :: Config -> IO Environment
buildEnvironment Config {..} = do
  let DbConnectInfo {..} = dbConnectInfo
  let connectInfo = ConnectInfo dbConnectHost dbConnectPort dbConnectUser dbConnectPassword dbConnectDatabase
  conn <- connect connectInfo
  pure $ Environment conn logInfo

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <-
    catch
      (BS.readFile "config.json")
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn $ "!!! Warning: Couldn't open: " ++ err
          return ""
      )
  pure $ decodeStrict rawJSON

argProcessing :: String -> ReaderT Environment IO ()
argProcessing "m" = execMigrations
argProcessing "f" = execFixtures
argProcessing arg = addLog DEBUG $ "unknown flag : " ++ arg

execMigrations :: ReaderT Environment IO ()
execMigrations = do
  conn <- asks dbConnection
  schemaRes <-
    lift $
      withTransaction conn $
        runMigration $
          MigrationContext MigrationInitialization True conn
  addLog DEBUG $ "execSchemaMigrations : " ++ show schemaRes
  res <-
    lift $
      withTransaction conn $
        runMigration $
          MigrationContext (MigrationDirectory "migrations") True conn
  addLog DEBUG $ "execMigrations : " ++ show res

execFixtures :: ReaderT Environment IO ()
execFixtures = do
  conn <- asks dbConnection
  res <-
    lift $
      withTransaction conn $
        runMigration $
          MigrationContext (MigrationDirectory "fixtures") True conn
  addLog DEBUG $ "execFixtures : " ++ show res
