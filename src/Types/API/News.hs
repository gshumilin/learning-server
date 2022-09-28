{-# LANGUAGE DeriveAnyClass #-}
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
  deriving (Generic, FromJSON, Show)

data EditNewsRequest = EditNewsRequest
  { newsId :: Int,
    newTitle :: Maybe T.Text,
    newCategoryId :: Maybe Int,
    newTextContent :: Maybe T.Text,
    newPictures :: Maybe [Domain.Picture],
    newPublishStatus :: Maybe Bool
  }
  deriving (Generic, FromJSON, Show)
