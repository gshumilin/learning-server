module Endpoints.GetCategories where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (find)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.Category (parseCategoriesList, readCategoryWithParentsById)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Response, responseLBS)
import qualified Types.DB.Category as DBType
import qualified Types.Domain.Category as Domain
import Types.Domain.Environment

getCategories :: ReaderT Environment IO Response
getCategories = do
  conn <- asks dbConnection
  dbCatList <- lift $ parseCategoriesList conn
  catList <- mapM (dbCategoryTransform . DBType.categoryID) dbCatList
  let jsonNewsList = encodePretty catList
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonNewsList

getSpecificCategory :: Connection -> Int -> IO (Maybe Domain.Category)
getSpecificCategory conn cid = do
  categoryWithParrents <- readCategoryWithParentsById cid conn
  pure $ fromDbCategoryList categoryWithParrents

dbCategoryTransform :: Int -> ReaderT Environment IO Domain.Category
dbCategoryTransform dbCatId = do
  conn <- asks dbConnection
  dbCategoriesList <- lift $ parseCategoriesList conn
  let mbDbCat = find (\c -> DBType.categoryID c == dbCatId) dbCategoriesList
  case mbDbCat of
    Nothing -> error "dbCategoryTransform didn't find dbCategory"
    Just DBType.Category {..} -> do
      case parentID of
        Nothing -> pure $ Domain.Category categoryID title Nothing
        Just _ -> do
          let mbDbParentCat = find (\_ -> categoryID == 1) dbCategoriesList
          case mbDbParentCat of
            Nothing -> pure $ Domain.Category categoryID title Nothing
            Just dbParentCat -> do
              parentCat <- dbCategoryTransform (DBType.categoryID dbParentCat)
              pure $ Domain.Category categoryID title (Just parentCat)

fromDbCategoryList :: [DBType.Category] -> Maybe Domain.Category
fromDbCategoryList [] = Nothing
fromDbCategoryList (x : xs) =
  case DBType.parentID x of
    Nothing -> Just $ Domain.Category (DBType.categoryID x) (DBType.title x) Nothing
    Just _ -> Just $ Domain.Category (DBType.categoryID x) (DBType.title x) parentCategory
      where
        parentCategory = fromDbCategoryList xs
