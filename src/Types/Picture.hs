module Types.Picture where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types

data Picture = Picture 
    { base64 :: T.Text
    }

instance FromJSON Picture where 
    parseJSON (Object inputJSON) = undefined

instance ToJSON Picture where
    toJSON = undefined