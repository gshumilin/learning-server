module DataBaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple

findPicturesArray :: Connection -> Int -> IO (Maybe [Picture])
findPicturesArray conn newsID = do
    let q = "SELECT p.base64 FROM news_pictures np LEFT JOIN pictures p on p.id=np.picture_id WHERE np.news_id = ?"
    res <- query conn q $ Only newsID
    if not . null $ res
        then return $ Just res
        else return Nothing