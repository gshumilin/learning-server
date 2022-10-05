{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Domain.Picture where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, (.:), (.=))
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Picture = Picture
  { picData :: T.Text,
    mime :: T.Text
  }
  deriving (Show, Generic, FromRow)

instance FromJSON Picture where
  parseJSON (Object inputJSON) = do
    picture <- inputJSON .: "image"
    picData <- picture .: "data"
    mime <- picture .: "mime"
    pure $ Picture picData mime
  parseJSON _ = mzero

instance ToJSON Picture where
  toJSON Picture {..} =
    object
      [ "data" .= picData,
        "mime" .= mime
      ]
