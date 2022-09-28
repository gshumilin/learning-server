{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Picture where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, (.:), (.=))
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

newtype Pictures = Pictures [Picture] deriving (Generic, Show, FromJSON, ToJSON)

data Picture = Picture
  { mime :: T.Text,
    picData :: T.Text
  }
  deriving (Show, Generic, FromRow)

instance FromJSON Picture where
  parseJSON (Object inputJSON) = do
    picture <- inputJSON .: "image"
    mime <- picture .: "mime"
    picData <- picture .: "data"
    pure $ Picture mime picData
  parseJSON _ = mzero

instance ToJSON Picture where
  toJSON Picture {..} =
    object
      [ "mime" .= mime,
        "data" .= picData
      ]
