module Types.Domain.Log where

import Data.Aeson.Types
import Data.Text (toUpper)


-- DEBUG — логирование всех событий при отладке.
-- WARNING — логирование ошибок и предупреждений.
-- RELEASE — логироване только тотальных ошибок (например "нет связи с базой данных")

data LogLvl = DEBUG | WARNING | RELEASE deriving (Show, Eq, Ord)

instance FromJSON LogLvl where
    parseJSON (String txt) 
        | toUpper txt == "DEBUG" = return DEBUG
        | toUpper txt == "WARNING" = return WARNING
        | toUpper txt == "RELEASE" = return RELEASE
    --  | otherwise = ???