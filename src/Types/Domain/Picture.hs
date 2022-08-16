module Types.Domain.Picture where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Data.ByteString as BS 

data Pictures = Pictures [Picture] deriving Show

instance FromJSON Pictures where 
    parseJSON (Object inputJSON) = do
        arr <- inputJSON .: "pictures"
        return $ Pictures arr

instance ToJSON Pictures where
    toJSON (Pictures arr) = 
        object  [ "pictures" .= arr
                ]
                
instance ToField Pictures where
    toField = undefined

data Picture = Picture 
    {   mime :: T.Text,
        picData :: T.Text
    } deriving Show

instance FromJSON Picture where 
    parseJSON (Object inputJSON) = do
        picture <- inputJSON .: "image"
        mime <- picture .: "mime"
        picData <- picture .: "data"
        return Picture {..}

instance ToJSON Picture where
    toJSON Picture {..} = 
        object  [ "data" .= picData
                ]

instance FromRow Picture where
    fromRow = do
        picData <- field
        mime <- field
        return Picture {..}

instance ToField Picture where
    toField = undefined