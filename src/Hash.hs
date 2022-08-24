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


pass :: BS.ByteString
pass = passHashBS "admin"

-- 1aa66b393b8ab47d2489de43016f5f39ca8c2412737c6cdc5f7bc33de2f6dbed
-- 1aa66b393b8ab47d2489de43016f5f39ca8c2412737c6cdc5f7bc33de2f6dbed
-- 1aa66b393b8ab47d2489de43016f5f39ca8c2412737c6cdc5f7bc33de2f6dbed