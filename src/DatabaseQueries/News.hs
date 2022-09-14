module DatabaseQueries.News where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import DatabaseQueries.Picture (addPicturesToNews, deleteNewsPictures, parsePicturesLinks)
import DatabaseQueries.QueryCreator (makeReadNewsQuery)
import DatabaseQueries.User (findUser)
import Endpoints.GetCategories (getSpecificCategory)
import Log (addLog)
import Network.Wai (Request)
import qualified Types.API.News as API (CreateNewsRequest (..), EditNewsRequest (..))
import qualified Types.DB.News as DB (EditedNewsFields (..), News (..))
import Types.Domain.Environment (Environment (..))
import Types.Domain.Log (LogLvl (..))
import qualified Types.Domain.News as Domain (News (..))
import qualified Types.Domain.Picture as Domain (Picture (..))

readNews :: Request -> ReaderT Environment IO [Domain.News]
readNews req = do
  addLog DEBUG "----- Started readNews \n"
  conn <- asks dbConnection
  mbQuery <- lift $ makeReadNewsQuery conn req
  case mbQuery of
    Nothing -> pure []
    Just q -> do
      let (Query bsQ) = q
      addLog DEBUG $ "----- made this psql-request: \n\"" <> T.decodeUtf8 bsQ
      dbNews <- lift $ query_ conn q :: ReaderT Environment IO [DB.News]
      addLog DEBUG $ "----- got this psql News List: \"" <> T.pack (show dbNews)
      lift $ mapM (fromDbNews conn) dbNews

fromDbNews :: Connection -> DB.News -> IO Domain.News
fromDbNews conn DB.News {..} = do
  newsCategory <- getSpecificCategory conn categoryId
  newsCreator <- findUser conn creatorId
  newsPictures <- parsePicturesLinks conn newsId
  pure $
    Domain.News
      { newsId = newsId,
        title = title,
        createDate = createDate,
        creator = newsCreator,
        category = newsCategory,
        textContent = textContent,
        pictures = newsPictures,
        isPublished = isPublished,
        numbersOfPictures = numbersOfPictures
      }

readSpecificNews :: Connection -> Int -> IO (Maybe DB.EditedNewsFields)
readSpecificNews conn newsId = do
  let q = "SELECT creator_id, title, category_id, text_content FROM news WHERE id=?"
  res <- query conn q (Only newsId) :: IO [DB.EditedNewsFields]
  case res of
    [] -> pure Nothing
    [news] -> pure $ Just news
    (news : _) -> pure $ Just news

writeNews :: Connection -> Int -> API.CreateNewsRequest -> IO ()
writeNews conn newsCreatorId API.CreateNewsRequest {..} = do
  currTime <- getCurrentTime
  let isPublished = False
  let q = "INSERT INTO news (title, create_date, creator_id, category_id, text_content, is_published) values (?,?,?,?,?,?) RETURNING id"
  [Only newsId] <- query conn q (title, currTime, newsCreatorId, categoryId, textContent, isPublished) :: IO [Only Int]
  case pictures of
    Nothing -> pure ()
    Just picArr -> do
      mapM_
        ( \Domain.Picture {..} -> do
            let insertingQ = "INSERT INTO pictures (data,mime) values (?,?) RETURNING id"
            [Only picId] <- query conn insertingQ (picData, mime) :: IO [Only Int]
            let insertingQ' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
            execute conn insertingQ' (newsId, picId)
        )
        picArr

rewriteNews :: Connection -> DB.EditedNewsFields -> API.EditNewsRequest -> IO ()
rewriteNews conn editedNewsFields editNewsRequest = do
  let q = "UPDATE news SET title=?, category_id=?, text_content=? WHERE id=?"
  _ <-
    execute
      conn
      q
      ( fromMaybe (DB.oldTitle editedNewsFields) (API.newTitle editNewsRequest),
        fromMaybe (DB.oldCategoryId editedNewsFields) (API.newCategoryId editNewsRequest),
        fromMaybe (DB.oldTextContent editedNewsFields) (API.newTextContent editNewsRequest),
        API.newsId editNewsRequest
      )
  case API.newPictures editNewsRequest of
    Nothing -> pure ()
    Just [] -> do
      deleteNewsPictures conn (API.newsId editNewsRequest)
      pure ()
    Just picArr -> do
      deleteNewsPictures conn (API.newsId editNewsRequest)
      addPicturesToNews conn (API.newsId editNewsRequest) picArr
      pure ()
