module Utils.TypeConverter where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString, fromStrict)

intToLBS :: Int -> ByteString
intToLBS = fromStrict . pack . show
