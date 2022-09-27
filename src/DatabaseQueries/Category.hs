module DatabaseQueries.Category where

import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_)
import qualified Types.API.Category as API (CreateCategoryRequest (..), EditCategoryRequest (..))
import qualified Types.DB.Category as DB (Category (..))

parseCategoriesList :: Connection -> IO [DB.Category]
parseCategoriesList conn = query_ conn "SELECT * FROM categories"

readCategoryById :: Connection -> Int -> IO (Maybe DB.Category)
readCategoryById conn catId = do
  let q =
        " SELECT * FROM categories \
        \ WHERE id=?"
  res <- query conn q (Only catId)
  case res of
    [] -> pure Nothing
    (c : _) -> pure $ Just c

readCategoryByTitle :: Connection -> T.Text -> IO (Maybe DB.Category)
readCategoryByTitle conn title = do
  let q =
        " SELECT * FROM categories \
        \ WHERE title=?"
  res <- query conn q (Only title)
  case res of
    [] -> pure Nothing
    (c : _) -> pure $ Just c

readCategoryWithParentsById :: Int -> Connection -> IO [DB.Category]
readCategoryWithParentsById catId conn = do
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
  query conn q (Only catId)

writeCategory :: Connection -> API.CreateCategoryRequest -> IO ()
writeCategory conn API.CreateCategoryRequest {..} = do
  let q =
        " INSERT INTO categories \
        \ (title,parent_category_id) \
        \ VALUES (?,?)"
  _ <- execute conn q (title, parentCategoryId)
  pure ()

rewriteCategory :: Connection -> API.EditCategoryRequest -> IO ()
rewriteCategory conn API.EditCategoryRequest {..} = do
  _ <- execTitle newTitle
  _ <- execParent newParentCategoryId
  pure ()
  where
    execTitle (Just t) =
      let q = "UPDATE categories SET title = ? WHERE id = ?"
       in execute conn q (t, processedCategoryId)
    execTitle Nothing = pure 0

    execParent (Just 0) =
      let q = "UPDATE categories SET parent_category_id = NULL WHERE id = ?"
       in execute conn q (Only processedCategoryId)
    execParent (Just parId) =
      let q = "UPDATE categories SET parent_category_id = ? WHERE id = ?"
       in execute conn q (parId, processedCategoryId)
    execParent Nothing = pure 0
