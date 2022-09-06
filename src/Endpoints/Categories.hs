module Endpoints.Categories where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Aeson (decodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (find)
import Database.PostgreSQL.Simple (Connection)
import DatabaseQueries.Category (parseCategoriesList, readCategoryWithParentsById, rewriteCategory, writeCategory)
import Network.HTTP.Types (hContentType, status200, status400)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)
import qualified Types.API.Category as API
import qualified Types.DB.Category as DBType
import qualified Types.Domain.Category as Domain
import Types.Domain.Environment

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

getCategoriesList :: ReaderT Environment IO Response
getCategoriesList = do
  conn <- asks dbConnection
  dbCatList <- lift $ parseCategoriesList conn
  catList <- mapM (dbCategoryTransform . DBType.categoryID) dbCatList
  let jsonNewsList = encodePretty catList
  pure $ responseLBS status200 [(hContentType, "text/plain")] jsonNewsList

fromDbCategoryList :: [DBType.Category] -> Maybe Domain.Category
fromDbCategoryList [] = Nothing
fromDbCategoryList (x : xs) =
  case DBType.parentID x of
    Nothing -> Just $ Domain.Category (DBType.categoryID x) (DBType.title x) Nothing
    Just _ -> Just $ Domain.Category (DBType.categoryID x) (DBType.title x) parentCategory
      where
        parentCategory = fromDbCategoryList xs

getSpecificCategory :: Connection -> Int -> IO (Maybe Domain.Category)
getSpecificCategory conn cid = do
  categoryWithParrents <- readCategoryWithParentsById cid conn
  pure $ fromDbCategoryList categoryWithParrents

createCategory :: Request -> ReaderT Environment IO Response
createCategory request = do
  conn <- asks dbConnection
  rawJSON <- lift $ getRequestBodyChunk request
  let decodedReq = decodeStrict rawJSON :: Maybe API.CreateCategoryRequest
  case decodedReq of
    Nothing -> do
      lift $ putStrLn "Invalid JSON" -- log
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just newCategory -> do
      lift $ writeCategory conn newCategory
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"

editCategory :: Request -> ReaderT Environment IO Response
editCategory request = do
  conn <- asks dbConnection
  rawJSON <- lift $ getRequestBodyChunk request
  let decodedReq = decodeStrict rawJSON :: Maybe API.EditCategoryRequest
  case decodedReq of
    Nothing -> do
      lift $ putStrLn "Invalid JSON"
      pure $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request: Invalid JSON\n"
    Just editedCategory -> do
      lift $ rewriteCategory conn editedCategory
      pure $ responseLBS status200 [(hContentType, "text/plain")] "all done"
