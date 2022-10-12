module DatabaseQueries.News where

import Control.Monad.Reader (ReaderT, lift)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Only (..), execute, query, query_)
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
import Utils.Pool (withPool)

readNews :: Request -> ReaderT Environment IO [Domain.News]
readNews req = do
  addLog DEBUG "----- Started readNews \n"
  mbQuery <- makeReadNewsQuery req
  case mbQuery of
    Nothing -> pure []
    Just q -> do
      let (Query bsQ) = q
      addLog DEBUG $ "----- made this psql-request: \n\"" <> T.decodeUtf8 bsQ
      dbNews <- withPool $ \conn -> query_ conn q :: IO [DB.News]
      addLog DEBUG $ "----- got this psql News List: \"" <> T.pack (show dbNews)
      mapM fromDbNews dbNews

fromDbNews :: DB.News -> ReaderT Environment IO Domain.News
fromDbNews DB.News {..} = do
  newsCategory <- getSpecificCategory categoryId
  newsCreator <- findUser creatorId
  newsPictures <- parsePicturesLinks newsId
  pure $
    Domain.News
      { newsId = newsId,
        title = title,
        createDate = createDate,
        creator = newsCreator,
        category = newsCategory,
        textContent = textContent,
        pictures = newsPictures,
        isPublished = isPublished
      }

readSpecificNews :: Int -> Int -> ReaderT Environment IO (Maybe DB.EditedNewsFields)
readSpecificNews userId newsId = do
  let q =
        " SELECT title, category_id, text_content, is_published \
        \ FROM news WHERE id=? AND creator_id=?"
  res <- withPool $ \conn -> query conn q (newsId, userId) :: IO [DB.EditedNewsFields]
  case res of
    [] -> pure Nothing
    [news] -> pure $ Just news
    (news : _) -> pure $ Just news

writeNews :: Int -> API.CreateNewsRequest -> ReaderT Environment IO Int
writeNews newsCreatorId API.CreateNewsRequest {..} = do
  currTime <- lift getCurrentTime
  let isPublished = False
  let q =
        " INSERT INTO news \
        \ (title, create_date, creator_id, category_id, text_content, is_published) \
        \ VALUES (?,?,?,?,?,?) \
        \ RETURNING id"
  [Only newsId] <- withPool $ \conn -> query conn q (title, currTime, newsCreatorId, categoryId, textContent, isPublished) :: IO [Only Int]
  case pictures of
    Nothing -> pure newsId
    Just picArr -> do
      mapM_
        ( \Domain.Picture {..} -> do
            let picQ = "INSERT INTO pictures (data, mime) VALUES (?,?) RETURNING id"
            [Only picId] <- withPool $ \conn -> query conn picQ (picData, mime) :: IO [Only Int]
            let newsPicQ = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
            withPool $ \conn -> execute conn newsPicQ (newsId, picId)
        )
        picArr
      pure newsId

rewriteNews :: DB.EditedNewsFields -> API.EditNewsRequest -> ReaderT Environment IO ()
rewriteNews editedNewsFields editNewsRequest = do
  let q = "UPDATE news SET title=?, category_id=?, text_content=?, is_published=? WHERE id=?"
  _ <- withPool $ \conn ->
    execute
      conn
      q
      ( fromMaybe (DB.oldTitle editedNewsFields) (API.newTitle editNewsRequest),
        fromMaybe (DB.oldCategoryId editedNewsFields) (API.newCategoryId editNewsRequest),
        fromMaybe (DB.oldTextContent editedNewsFields) (API.newTextContent editNewsRequest),
        fromMaybe (DB.oldPublishStatus editedNewsFields) (API.newPublishStatus editNewsRequest),
        API.newsId editNewsRequest
      )
  case API.newPictures editNewsRequest of
    Nothing -> pure ()
    Just [] -> do
      deleteNewsPictures (API.newsId editNewsRequest)
      pure ()
    Just picArr -> do
      deleteNewsPictures (API.newsId editNewsRequest)
      addPicturesToNews (API.newsId editNewsRequest) picArr
      pure ()

findNewsIdByTitle :: T.Text -> ReaderT Environment IO (Maybe Int)
findNewsIdByTitle title = do
  let q =
        " SELECT id\
        \ FROM news WHERE title=?"
  res <- withPool $ \conn -> query conn q (Only title) :: IO [Only Int]
  case res of
    [] -> pure Nothing
    [Only i] -> pure $ Just i
    (Only i : _) -> pure $ Just i
