module DataBaseQueries.Picture where

import Types.Domain.Picture
import Database.PostgreSQL.Simple

findPicturesArray :: Connection -> Int -> IO (Maybe [Picture])
findPicturesArray conn pictureID = do
    res <- query conn "SELECT base64 FROM pictures WHERE id = ?" $ Only pictureID
    if not . null $ res
        then return $ Just res
        else return Nothing