module Types.Domain.Log where

import qualified Data.Text as T
import Data.Aeson.Types
import Data.Text (toUpper)

-- DEBUG    — логирование всех событий при отладке.
-- WARNING  — логирование ошибок и предупреждений.
-- RELEASE  — логироване только тотальных ошибок (например "нет связи с базой данных")

data LogInfo = LogInfo
    {   logLvl :: LogLvl,
        logPath :: T.Text
    }

instance FromJSON LogInfo where
    parseJSON (Object inputJSON) = do
        logLvl <- inputJSON .: "logLvl"
        logPath <- inputJSON .: "logPath"
        return LogInfo {..} 

data LogLvl = DEBUG | WARNING | RELEASE deriving (Show, Eq, Ord)

instance FromJSON LogLvl where
    parseJSON (String txt) 
        | toUpper txt == "DEBUG" = return DEBUG
        | toUpper txt == "WARNING" = return WARNING
        | toUpper txt == "RELEASE" = return RELEASE
    --  | otherwise = ???