module Endpoints.GetCategories where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson.Encode.Pretty (encodePretty)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.Category (parseCategoriesList, readCategoryWithParentsById)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Response, responseLBS)
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.Domain.Category as Domain (Category (..))
import Types.Domain.Environment (Environment (..))

getCategories :: ReaderT Environment IO Response
getCategories = do
  conn <- asks dbConnection
  dbCatList <- lift $ parseCategoriesList conn
  let jsonNewsList = encodePretty dbCatList
  pure $ responseLBS status200 [(hContentType, "application/json")] jsonNewsList

getSpecificCategory :: Connection -> Int -> IO (Maybe Domain.Category)
getSpecificCategory conn cid = do
  categoryWithParrents <- readCategoryWithParentsById cid conn
  pure $ fromDbCategoryList categoryWithParrents

fromDbCategoryList :: [DB.Category] -> Maybe Domain.Category
fromDbCategoryList [] = Nothing
fromDbCategoryList (x : xs) =
  case DB.parentID x of
    Nothing -> Just $ Domain.Category (DB.categoryID x) (DB.title x) Nothing
    Just _ -> Just $ Domain.Category (DB.categoryID x) (DB.title x) parentCategory
      where
        parentCategory = fromDbCategoryList xs
