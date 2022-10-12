module Main where

import Control.Exception (IOException, SomeException, catch)
import Control.Exception.Base (try)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (PoolConfig (..), newPool)
import qualified Data.Text as T (pack)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connect, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), runMigration)
import Log (addLog, makeLogDesc)
import Network.Wai.Handler.Warp (run)
import Routing (application)
import System.Environment (getArgs)
import Types.Domain.Environment (Config (..), DbConnectInfo (..), Environment (..))
import Types.Domain.Log (LogLvl (..))
import Utils.Pool (withPool)

main :: IO ()
main = do
  mbConf <- getConfig
  case mbConf of
    Nothing -> putStrLn "Config wasn't parsed! Server wasn't started"
    Just conf -> do
      isInvalid <- checkConnectInfo conf
      if isInvalid
        then pure ()
        else do
          env <- buildEnvironment conf
          let port = serverPort conf
          let app req respond = runReaderT (application req respond) env
          args <- getArgs
          runReaderT (mapM_ argProcessing args) env
          runReaderT (addLog RELEASE "_____ Server started _____") env
          runReaderT (addLog DEBUG ("port = " <> T.pack (show port))) env
          run port app

buildEnvironment :: Config -> IO Environment
buildEnvironment Config {..} = do
  let DbConnectInfo {..} = dbConnectInfo
  let connectInfo = ConnectInfo dbConnectHost dbConnectPort dbConnectUser dbConnectPassword dbConnectDatabase
  let poolConfig =
        PoolConfig
          { createResource = connect connectInfo,
            freeResource = close,
            poolCacheTTL = 0.5,
            poolMaxResources = 10
          }
  pool <- newPool poolConfig
  logDescriptor <- makeLogDesc logDescType
  pure $ Environment pool domain logLvl logDescriptor

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

checkConnectInfo :: Config -> IO Bool
checkConnectInfo Config {..} = do
  let DbConnectInfo {..} = dbConnectInfo
  let connectInfo = ConnectInfo dbConnectHost dbConnectPort dbConnectUser dbConnectPassword dbConnectDatabase
  res <- try (connect connectInfo) :: IO (Either SomeException Connection)
  case res of
    Left err -> do
      putStrLn $ "Checking Connect Info: " ++ show err
      pure True
    Right _ -> do
      putStrLn "Checking Connect Info: OK "
      pure False

argProcessing :: String -> ReaderT Environment IO ()
argProcessing "m" = execMigrations
argProcessing "f" = execFixtures
argProcessing arg = addLog DEBUG $ "unknown flag : " <> T.pack (show arg)

execMigrations :: ReaderT Environment IO ()
execMigrations = do
  schemaRes <-
    withPool $ \conn ->
      withTransaction conn $
        runMigration $
          MigrationContext MigrationInitialization True conn
  addLog DEBUG $ "execSchemaMigrations : " <> T.pack (show schemaRes)
  res <-
    withPool $ \conn ->
      withTransaction conn $
        runMigration $
          MigrationContext (MigrationDirectory "migrations") True conn
  addLog DEBUG $ "execMigrations : " <> T.pack (show res)

execFixtures :: ReaderT Environment IO ()
execFixtures = do
  res <-
    withPool $ \conn ->
      withTransaction conn $
        runMigration $
          MigrationContext (MigrationDirectory "fixtures") True conn
  addLog DEBUG $ "execFixtures : " <> T.pack (show res)
