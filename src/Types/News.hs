module Types.News where

import Types.User
import Types.Picture
import Types.Category
import Data.Time.Clock
import qualified Data.Text as T

data News = News
    { header :: T.Text,
      createDate :: UTCTime,
      creator :: User,
      categogy :: Category T.Text,
      content :: T.Text,
      picturesArray :: [Picture],
      isPublished :: Bool
    }