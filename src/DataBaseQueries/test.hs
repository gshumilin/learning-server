import Types.Domain.User
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import DataBaseQueries.GetConnection (getConnection)
import qualified Data.ByteString as B


main'' :: IO ()
main'' = do
    conn <- getConnection
    res <- parseUser' conn qq' :: IO [User]
    print res

qq' :: Query
qq' = "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users " <> "WHERE id=" <> "1"

parseUser' :: FromRow r => Connection -> Query -> IO [r]
parseUser' conn q = query_ conn q


-----------------------------------------------------------------------------------------

-- editNews ls newsID = Query $ "UPDATE news SET " <>  buildChanges ls <> " WHERE news_id = " <> newsID

-- buildChanges = undefined

main' :: IO ()
main' = do
    conn <- getConnection
    let q = initQuery `addFilter` Just "id<4" `addSort` Just "name DESC"
    res <- parseUser conn q :: IO [User]
    print res

parseUser :: FromRow r => Connection -> Query -> IO [r]
parseUser conn q = query_ conn q

initQuery :: Query
initQuery = Query $ "SELECT name,login,password,create_date,is_admin,is_able_to_create_news FROM users"

addSort :: Query -> Maybe Query -> Query
addSort q Nothing = q
addSort q (Just sortVal) = q <> " ORDER BY " <> sortVal

addFilter :: Query -> Maybe Query -> Query
addFilter q Nothing = q
addFilter q (Just filterVal) = q <> " WHERE " <> filterVal

ls = Query $ "select *" <> Nothing