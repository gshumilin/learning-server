module Main where

import Routing (routing)
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
    let appFunc = runReaderT (routing) env
    run port appFunc

buildEnvironment :: Config -> IO (Environment)
buildEnvironment conf = do
    conn <- connect . dbConnectInfo $ conf
    return $ Environment conn

getConfig :: IO (Config)
getConfig = do
    rawJSON <- BS.readFile "config.json"
    case decodeStrict rawJSON of
        Nothing -> error "Config cannot be read. Invalid JSON"
        Just conf -> return $ conf