module Types.Picture where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types

data Picture = Picture 
    { base64 :: T.Text
    }

instance FromJSON Picture where 
    parseJSON (Object inputJSON) = do
        base64 <- inputJSON .: "base64"
        return Picture {..}

instance ToJSON Picture where
    toJSON Picture {..} = 
        object  [ "base64" .= base64
                ]