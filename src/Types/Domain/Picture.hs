module Types.Domain.Picture where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString as BS
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

newtype Pictures = Pictures [Picture] deriving (Show)

instance FromJSON Pictures where
  parseJSON (Object inputJSON) = do
    arr <- inputJSON .: "pictures"
    pure $ Pictures arr
  parseJSON _ = mzero

instance ToJSON Pictures where
  toJSON (Pictures arr) =
    object
      [ "pictures" .= arr
      ]

instance ToField Pictures where
  toField = undefined

data Picture = Picture
  { mime :: T.Text,
    picData :: T.Text
  }
  deriving (Show)

instance FromJSON Picture where
  parseJSON (Object inputJSON) = do
    picture <- inputJSON .: "image"
    mime <- picture .: "mime"
    picData <- picture .: "data"
    pure $ Picture mime picData

instance ToJSON Picture where
  toJSON Picture {..} =
    object
      [ "mime" .= mime,
        "data" .= picData
      ]

instance FromRow Picture where
  fromRow = do
    picData <- field
    mime <- field
    pure Picture {..}

instance ToField Picture where
  toField = undefined
