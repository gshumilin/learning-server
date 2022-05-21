{-# LANGUAGE OverloadedStrings #-}

module Endpoints.User where

import Types.User
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar
import Network.HTTP.Types (hContentType, status200)
import Network.Wai
import Data.Aeson.Encode.Pretty (encodePretty)
import ToJSON

getUsersList :: Response
getUsersList =
    responseLBS status200 [(hContentType, "text/plain")] $ renderedUsersList
    where
        renderedUsersList = encodePretty usersList

createUser :: Request -> Response
createUser req = 
    responseLBS status200 [(hContentType, "text/plain")] $ fx reqQuery
    where 
        reqQuery = rawQueryString req
        fx :: BS.ByteString -> LBS.ByteString
        fx rq = undefined

usersList = UsersList $
            [   User { name = "Gena Shumilin",
                       login = "1000-7_Geneki_7-1000",
                       password = "pleasedonthackme",
                       createDate = fromGregorian 2022 05 20,
                       isAdmin = True,
                       isAbleToCreateNews = True
                     } 
                    ,
                User { name = "Oleg Romashin",
                       login = "Colossus_Berutorutoleg",
                       password = "pleasedonthackme2",
                       createDate = fromGregorian 2022 05 20,
                       isAdmin = False,
                       isAbleToCreateNews = True
                     }
            ]