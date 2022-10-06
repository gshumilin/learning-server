module DatabaseQueries.QueryCreator where

import Auth (authorization)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isJust)
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Types (Query (..))
import Network.Wai (Request, queryString)
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Environment (Environment (..))

--Запрос формируется из секций init, filters, group, sorts, limit, offset. Их тип Maybe Query.
--программа игнорирует пустые поля и поля с некорректными названиями фильтров и параметров сортировки в запросе.
--на выходе получается что-то такое
--   SELECT news.id,
--      news.title,
--      news.create_date,
--      news.creator_id,
--      users.login,
--      news.category_id,
--      categories.title,
--      news.text_content,
--      news.is_published,
--     FROM news
--     JOIN users on news.creator_id=users.id
--     JOIN categories on news.category_id=categories.id
--  LEFT JOIN news_pictures on news_pictures.news_id=news.id
--    WHERE (is_published = True OR creator_id = 1)
--   GROUP BY news.id,
--      news.title,
--      news.create_date,
--      news.creator_id,
--      users.login,
--      news.category_id,
--      categories.title,
--      news.text_content,
--      news.is_published
-- ORDER BY COUNT (news_pictures.picture_id)"

makeReadNewsQuery :: Request -> ReaderT Environment IO (Maybe Query)
makeReadNewsQuery req = do
  clientUser <- authorization req
  let initQ = makeInitReadNewsQuery clientUser
  let filtersQuery =
        makeFiltersQuery
          . findCompletedFields
            [ "creator_login",
              "category_title",
              "created_at",
              "created_until",
              "created_since",
              "title",
              "content"
            ]
          . queryString
          $ req
  let groupQ =
        Query $
          "  GROUP BY news.id,\n"
            <> "       news.title,\n"
            <> "       news.create_date,\n"
            <> "       news.creator_id,\n"
            <> "       users.login,\n"
            <> "       news.category_id,\n"
            <> "       categories.title,\n"
            <> "       news.text_content,\n"
            <> "       news.is_published\n"
  let sortsQuery = makeSimpleQuery . findCompletedFields ["sort_by"] . queryString $ req
  let limitQuery = case findCompletedFields ["limit"] . queryString $ req of
        [] -> makeSimpleQuery [("limit", Just "10")]
        val -> makeSimpleQuery val
  let offsetQuery = makeSimpleQuery . findCompletedFields ["offset"] . queryString $ req
  let resultQ = Just initQ <> filtersQuery <> Just groupQ <> sortsQuery <> limitQuery <> offsetQuery

  pure resultQ

findCompletedFields :: [BS.ByteString] -> [(BS.ByteString, Maybe BS.ByteString)] -> [(BS.ByteString, Maybe BS.ByteString)]
findCompletedFields allowedNames = filter (\(fieldName, val) -> fieldName `elem` allowedNames && isJust val)

makeInitReadNewsQuery :: Either T.Text DB.User -> Query
makeInitReadNewsQuery dbUser =
  Query $
    "   SELECT news.id,\n"
      <> "       news.title,\n"
      <> "       news.create_date,\n"
      <> "       news.creator_id,\n"
      <> "       users.login,\n"
      <> "       news.category_id,\n"
      <> "       categories.title,\n"
      <> "       news.text_content,\n"
      <> "       news.is_published\n"
      <> "    FROM news\n"
      <> "    JOIN users on news.creator_id=users.id\n"
      <> "    JOIN categories on news.category_id=categories.id\n"
      <> " LEFT JOIN news_pictures on news_pictures.news_id=news.id\n"
      <> initWhereSection
  where
    initWhereSection = case dbUser of
      Left _ ->
        "   WHERE is_published = True\n"
      Right DB.User {..} ->
        "   WHERE (is_published = True OR creator_id = " <> BS.pack (show userId) <> ")\n"

makeFiltersQuery :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Query
makeFiltersQuery [] = Nothing
makeFiltersQuery ls = Just $ Query $ "       AND " <> BS.intercalate " AND " (foldr addParam [] ls) <> "\n"
  where
    addParam ("is_published", Just val) acc = ("is_published" <> " = " <> val) : acc
    addParam ("creator_id", Just val) acc = ("creator_id" <> " = " <> val) : acc
    addParam ("creator_login", Just val) acc = ("users.login" <> " = '" <> val <> "'") : acc
    addParam ("category_id", Just val) acc = ("category_id" <> " = " <> val) : acc
    addParam ("category_title", Just val) acc = ("categories.title" <> " = '" <> val <> "'") : acc
    addParam ("created_at", Just val) acc = ("news.create_date" <> " = '" <> val <> "'::timestamp") : acc
    addParam ("created_until", Just val) acc = ("news.create_date" <> " < '" <> val <> "'::timestamp") : acc
    addParam ("created_since", Just val) acc = ("news.create_date" <> " >= '" <> val <> "'::timestamp") : acc
    addParam ("title", Just val) acc = ("news.title" <> " LIKE '%" <> val <> "%'") : acc
    addParam ("content", Just val) acc = ("news.text_content" <> " LIKE '%" <> val <> "%'") : acc
    addParam _ _ = []

makeSimpleQuery :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe Query
makeSimpleQuery [("limit", Just val)] = Just $ Query $ " LIMIT " <> val
makeSimpleQuery [("offset", Just val)] = Just $ Query $ " OFFSET " <> val
makeSimpleQuery [("sort_by", Just "creator_login")] = Just $ Query " ORDER BY users.login\n"
makeSimpleQuery [("sort_by", Just "category_title")] = Just $ Query " ORDER BY categories.title\n"
makeSimpleQuery [("sort_by", Just "create_date")] = Just $ Query " ORDER BY create_date\n"
makeSimpleQuery [("sort_by", Just "number_of_pictures")] = Just $ Query "ORDER BY COUNT (news_pictures.picture_id)\n"
makeSimpleQuery [("sort_by", Just _)] = Nothing
makeSimpleQuery [(_, Nothing)] = Nothing
makeSimpleQuery [] = Nothing
makeSimpleQuery _ = Nothing
