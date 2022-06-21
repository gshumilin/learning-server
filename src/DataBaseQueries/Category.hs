module DataBaseQueries.Category where

import Types.Category
import qualified Types.Database.Category as DBType
import Database.PostgreSQL.Simple


parseCategoriesList :: Connection -> IO [(DBType.Category)]
parseCategoriesList conn = do
    res <- query_ conn "SELECT * FROM categories"
    return res