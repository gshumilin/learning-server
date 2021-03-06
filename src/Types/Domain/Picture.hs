module Types.Domain.Picture where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

data PicturesArray = PicturesArray [Picture] deriving Show

instance FromJSON PicturesArray where 
    parseJSON (Object inputJSON) = do
        arr <- inputJSON .: "picturesArray"
        return $ PicturesArray arr

instance ToJSON PicturesArray where
    toJSON (PicturesArray arr) = 
        object  [ "picturesArray" .= arr
                ]
                
instance ToField PicturesArray where
    toField = undefined

data Picture = Picture 
    { base64 :: T.Text
    } deriving Show

instance FromJSON Picture where 
    parseJSON (Object inputJSON) = do
        base64 <- inputJSON .: "base64"
        return Picture {..}

instance ToJSON Picture where
    toJSON Picture {..} = 
        object  [ "base64" .= base64
                ]

instance FromRow Picture where
    fromRow = do
        base64 <- field
        return Picture {..}

instance ToField Picture where
    toField = undefined