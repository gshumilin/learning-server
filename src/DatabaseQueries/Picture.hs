module DatabaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T

instance FromRow Int where
    fromRow = field

parsePicturesLinks :: Connection -> Int -> IO (Maybe [T.Text])
parsePicturesLinks conn newsID = do
    let q = "SELECT picture_id FROM news_pictures WHERE news_id = ?"
    res <- query conn q $ Only newsID :: IO ([Int])
    if null $ res
        then return Nothing
        else return . Just $ map makeLinks res
    where
        makeLinks :: Int -> T.Text
        makeLinks picId = "localhost:3000/getPicture?id=" <> (T.pack . show $ picId)

readPicture :: Connection -> Int -> IO (Maybe Picture)
readPicture conn pictureID = do
    let q = "SELECT data FROM pictures WHERE id = ?"
    res <- query conn q $ Only pictureID
    case res of
        [] -> return Nothing
        [x] -> return $ Just x

writePicture :: Connection -> Picture -> IO ()
writePicture conn Picture {..} = do
    execute conn "INSERT INTO pictures (data,mime) VALUES (?,?)" (picData,mime)
    return ()