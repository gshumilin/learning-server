module Types.Domain.News where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock
import qualified Types.Domain.Category as Domain
import Types.Domain.User

newtype NewsList = NewsList [News]

instance ToJSON NewsList where
  toJSON (NewsList list) =
    object
      [ "newsList" .= list
      ]

data News = News
  { newsID :: Int,
    title :: T.Text,
    createDate :: UTCTime,
    creator :: User,
    category :: Maybe Domain.Category,
    textContent :: T.Text,
    picturesLinks :: Maybe [T.Text],
    isPublished :: Bool,
    numbersOfPictures :: Int
  }
  deriving (Show)

instance FromJSON News where
  parseJSON (Object inputJSON) = do
    newsID <- inputJSON .: "newsID"
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
      [ "newsID" .= newsID,
        "title" .= title,
        "createDate" .= createDate,
        "creator" .= creator,
        "category" .= category,
        "textContent" .= textContent,
        "pictures" .= picturesLinks,
        "isPublished" .= isPublished,
        "numbersOfPictures" .= numbersOfPictures
      ]
