module Types.Domain.Category where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, (.:), (.=))
import qualified Data.Text as T

data Category = Category
  { categoryId :: Int,
    title :: T.Text,
    parent :: Maybe Category
  }
  deriving (Show)

instance FromJSON Category where
  parseJSON (Object o) = do
    categoryId <- o .: "categoryId"
    title <- o .: "title"
    parent <- o .: "parent"
    pure Category {..}
  parseJSON _ = mzero

instance ToJSON Category where
  toJSON Category {..} = do
    object
      [ "categoryId" .= categoryId,
        "title" .= title,
        "parent" .= parent
      ]
