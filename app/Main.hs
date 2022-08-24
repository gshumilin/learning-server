module Main where

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
    let app = \req respond -> runReaderT (application req respond) env
    run port app

buildEnvironment :: Config -> IO (Environment)
buildEnvironment Config {..} = do
    conn <- connect dbConnectInfo
    return $ Environment conn logInfo

getConfig :: IO (Config)
getConfig = do
    rawJSON <- BS.readFile "config.json"
    case decodeStrict rawJSON of
        Nothing -> error "Config cannot be read. Invalid JSON"
        Just conf -> return $ conf