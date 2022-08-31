module Main where

import Types.Domain.Log
import Log (addLog)
import Routing (application)
import Types.Domain.Environment
import Network.Wai.Handler.Warp (run)
import Data.Aeson (decodeStrict)
import Control.Monad.Reader
import Database.PostgreSQL.Simple (connect)
import qualified Data.ByteString.Char8 as BS

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
    conn <- connect dbConnectInfo
    pure $ Environment conn logInfo

getConfig :: IO Config
getConfig = do
    rawJSON <- BS.readFile "config.json"
    case decodeStrict rawJSON of
        Nothing -> error "Config cannot be read. Invalid JSON"
        Just conf -> pure conf