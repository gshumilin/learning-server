module DatabaseQueries.Category where

import Control.Monad.Reader (ReaderT)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only (..), execute, query, query_)
import qualified Types.API.Category as API (CreateCategoryRequest (..), EditCategoryRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import Types.Domain.Environment (Environment (..))
import Utils.Pool (withPool)

parseCategoriesList :: ReaderT Environment IO [DB.Category]
parseCategoriesList = withPool $ \conn -> query_ conn "SELECT * FROM categories"

readCategoryById :: Int -> ReaderT Environment IO (Maybe DB.Category)
readCategoryById catId = do
  let q =
        " SELECT * FROM categories \
        \ WHERE id=?"
  res <- withPool $ \conn -> query conn q (Only catId)
  case res of
    [] -> pure Nothing
    (c : _) -> pure $ Just c

readCategoryByTitle :: T.Text -> ReaderT Environment IO (Maybe DB.Category)
readCategoryByTitle title = do
  let q =
        " SELECT * FROM categories \
        \ WHERE title=?"
  res <- withPool $ \conn -> query conn q (Only title)
  case res of
    [] -> pure Nothing
    (c : _) -> pure $ Just c

readCategoryWithParentsById :: Int -> ReaderT Environment IO [DB.Category]
readCategoryWithParentsById catId = do
  let q =
        " WITH RECURSIVE records_list AS ( \
        \ SELECT n.id AS id,\
        \ n.title AS title,\
        \ n.parent_category_id AS parent_category_id,\
        \ 1 AS depth \
        \ FROM categories AS n \
        \ WHERE id = ? \
        \\
        \ UNION ALL\
        \\
        \ SELECT n.id AS id,\
        \ n.title AS title, \
        \ n.parent_category_id AS parent_category_id, \
        \ records_list.depth + 1 AS depth \
        \ FROM categories AS n \
        \\
        \ JOIN records_list ON records_list.parent_category_id = n.id ) \
        \\
        \ SELECT records_list.id, \
        \        records_list.title, \
        \        records_list.parent_category_id \
        \ FROM records_list ORDER BY depth;"
  withPool $ \conn -> query conn q (Only catId)

writeCategory :: API.CreateCategoryRequest -> ReaderT Environment IO Int
writeCategory API.CreateCategoryRequest {..} = do
  let q =
        " INSERT INTO categories \
        \ (title,parent_category_id) \
        \ VALUES (?,?) \
        \ RETURNING id"
  (Only resId : _) <- withPool $ \conn -> query conn q (title, parentCategoryId)
  pure resId

rewriteCategory :: API.EditCategoryRequest -> ReaderT Environment IO ()
rewriteCategory API.EditCategoryRequest {..} = do
  _ <- execTitle newTitle
  _ <- execParent newParentCategoryId
  pure ()
  where
    execTitle (Just t) =
      let q = "UPDATE categories SET title = ? WHERE id = ?"
       in withPool $ \conn -> execute conn q (t, processedCategoryId)
    execTitle Nothing = pure 0

    execParent (Just 0) =
      let q = "UPDATE categories SET parent_category_id = NULL WHERE id = ?"
       in withPool $ \conn -> execute conn q (Only processedCategoryId)
    execParent (Just parId) =
      let q = "UPDATE categories SET parent_category_id = ? WHERE id = ?"
       in withPool $ \conn -> execute conn q (parId, processedCategoryId)
    execParent Nothing = pure 0
