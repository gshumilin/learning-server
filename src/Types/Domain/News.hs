module Types.Domain.News where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, (.:), (.=))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import qualified Types.Domain.Category as Domain (Category (..))
import qualified Types.Domain.User as Domain (User (..))

newtype NewsList = NewsList [News]

instance ToJSON NewsList where
  toJSON (NewsList list) =
    object
      [ "newsList" .= list
      ]

data News = News
  { newsId :: Int,
    title :: T.Text,
    createDate :: UTCTime,
    creator :: Domain.User,
    category :: Maybe Domain.Category,
    textContent :: T.Text,
    picturesLinks :: Maybe [T.Text],
    isPublished :: Bool,
    numbersOfPictures :: Int
  }
  deriving (Show)

instance FromJSON News where
  parseJSON (Object inputJSON) = do
    newsId <- inputJSON .: "newsId"
    title <- inputJSON .: "title"
    createDate <- inputJSON .: "createDate"
    creator <- inputJSON .: "creator"
    category <- inputJSON .: "category"
    textContent <- inputJSON .: "textContent"
    picturesLinks <- inputJSON .: "pictures"
    isPublished <- inputJSON .: "isPublished"
    numbersOfPictures <- inputJSON .: "numbersOfPictures"
    pure $ News {..}
  parseJSON _ = mzero

instance ToJSON News where
  toJSON News {..} =
    object
      [ "newsId" .= newsId,
        "title" .= title,
        "createDate" .= createDate,
        "creator" .= creator,
        "category" .= category,
        "textContent" .= textContent,
        "pictures" .= picturesLinks,
        "isPublished" .= isPublished,
        "numbersOfPictures" .= numbersOfPictures
      ]
