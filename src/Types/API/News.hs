{-# LANGUAGE DeriveGeneric #-}

module Types.API.News where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Types.Domain.Picture as Domain (Picture (..))

data CreateNewsRequest = CreateNewsRequest
  { title :: T.Text,
    categoryId :: Int,
    textContent :: T.Text,
    pictures :: Maybe [Domain.Picture]
  }
  deriving (Generic, Show)

instance FromJSON CreateNewsRequest

data EditNewsRequest = EditNewsRequest
  { newsId :: Int,
    newTitle :: Maybe T.Text,
    newCategoryId :: Maybe Int,
    newTextContent :: Maybe T.Text,
    newPictures :: Maybe [Domain.Picture]
  }
  deriving (Generic, Show)

instance FromJSON EditNewsRequest
