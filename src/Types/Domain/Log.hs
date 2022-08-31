module Types.Domain.Log where

import Control.Monad (mzero)
import Data.Aeson.Types
import Data.Text (toUpper)
import qualified Data.Text as T

-- DEBUG    — логирование всех событий при отладке.
-- WARNING  — логирование ошибок и предупреждений.
-- RELEASE  — логироване только тотальных ошибок (например "нет связи с базой данных")

data LogInfo = LogInfo
  { logLvl :: LogLvl,
    logPath :: T.Text
  }

instance FromJSON LogInfo where
  parseJSON (Object inputJSON) = do
    logLvl <- inputJSON .: "logLvl"
    logPath <- inputJSON .: "logPath"
    pure LogInfo {..}

data LogLvl = DEBUG | WARNING | RELEASE deriving (Show, Eq, Ord)

instance FromJSON LogLvl where
  parseJSON (String txt)
    | toUpper txt == "DEBUG" = pure DEBUG
    | toUpper txt == "WARNING" = pure WARNING
    | toUpper txt == "RELEASE" = pure RELEASE
    | otherwise = mzero
