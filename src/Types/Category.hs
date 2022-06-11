module Types.Category where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types

data Category a = Empty | CatName a (Category a) deriving Show

exCategory :: Category String
exCategory = CatName "Anime" (CatName "Action/dramma" (CatName "Titan Attack" Empty))

instance FromJSON (Category a) where
    parseJSON (Object inputJSON) = undefined

instance ToJSON (Category a) where
    toJSON Empty = undefined
    toJSON (CatName x y) = undefined