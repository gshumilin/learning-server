module DatabaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple
import qualified Data.Text as T

findPictures :: Connection -> Int -> IO (Maybe Pictures)
findPictures conn newsID = do
    let q = "SELECT pictures.data FROM news_pictures LEFT JOIN pictures on pictures.id=news_pictures.picture_id WHERE news_pictures.news_id = ?"
    res <- query conn q $ Only newsID
    if not . null $ res
        then return $ Just (Pictures res)
        else return Nothing

readPicture :: Connection -> Int -> IO (Maybe Picture)
readPicture conn pictureID = do
    let q = "SELECT data FROM pictures WHERE id = ?"
    res <- query conn q $ Only pictureID
    case res of
        [] -> return Nothing
        [x] -> return $ Just x

writePicture :: Connection -> Picture -> IO ()
writePicture conn Picture {..} = do
    execute conn "INSERT INTO pictures (data) VALUES (?)" (Only picData)
    return ()