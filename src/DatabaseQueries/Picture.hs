module DatabaseQueries.Picture where

import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Types.Domain.Picture (Picture (..))

parsePicturesLinks :: Connection -> Int -> IO (Maybe [T.Text])
parsePicturesLinks conn newsId = do
  let q = "SELECT picture_id FROM news_pictures WHERE news_id = ?"
  res <- query conn q $ Only newsId :: IO [Only Int]
  if null res
    then pure Nothing
    else pure . Just $ map makeLinks res
  where
    makeLinks :: Only Int -> T.Text
    makeLinks (Only picId) = "localhost:3000/getPicture?id=" <> (T.pack . show $ picId)

readPicture :: Connection -> Int -> IO (Maybe Picture)
readPicture conn pictureId = do
  let q = "SELECT data, mime FROM pictures WHERE id = ?"
  res <- query conn q $ Only pictureId
  case res of
    [] -> pure Nothing
    (x : _) -> pure $ Just x

writePicture :: Connection -> Picture -> IO Int
writePicture conn Picture {..} = do
  let q =
        " INSERT INTO pictures (data,mime) \
        \ VALUES (?,?) RETURNING id"
  [Only resId] <- query conn q (picData, mime)
  pure resId

addPicturesToNews :: Connection -> Int -> [Picture] -> IO ()
addPicturesToNews conn newsId picArr = do
  mapM_
    ( \Picture {..} -> do
        let q = "INSERT INTO pictures (data,mime) values (?,?) pureING id"
        [Only picId] <- query conn q (picData, mime) :: IO [Only Int]
        let q' = "INSERT INTO news_pictures (news_id, picture_id) values (?,?)"
        execute conn q' (newsId, picId)
    )
    picArr

deleteNewsPictures :: Connection -> Int -> IO ()
deleteNewsPictures conn newsId = do
  picIdArray <- query conn "DELETE FROM news_pictures WHERE news_id=? pureING picture_id" (Only newsId) :: IO [Only Int]
  mapM_ (execute conn "DELETE FROM pictures WHERE id=?") picIdArray
