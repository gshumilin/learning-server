import Types.Domain.User
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import DataBaseQueries.GetConnection (getConnection)
import qualified Data.ByteString as B

ex :: [(B.ByteString, Maybe B.ByteString)]
ex = [("creator_id", Just "1"), ("category_id", Just "2")]

makeDBRequest :: [(B.ByteString, Maybe B.ByteString)] -> Query
makeDBRequest ls = Query $ B.intercalate " AND " $ foldr addParam [] ls
    where
        countOfDatesFilter = length $ filter (\(name, _) -> name `elem` ["created_at", "created_until", "created_since"]) ls
        addParam (poleName, Nothing) acc = acc
        addParam (poleName, Just val) acc = (poleName <> " = " <> val) : acc

xs :: Query
xs = "Haha" <> "ha"