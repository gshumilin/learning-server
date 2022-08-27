module Hash where

import Crypto.Hash (hash, Digest(..), Blake2b_256)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)

passHashT :: T.Text -> T.Text
passHashT p = T.pack . show $ hashedPass
    where 
        hashedPass = hash . T.encodeUtf8 $ p :: Digest Blake2b_256

passHashBS :: BS.ByteString -> BS.ByteString
passHashBS p = BS.pack . show $ hashedPass
    where 
        hashedPass = hash p :: Digest Blake2b_256