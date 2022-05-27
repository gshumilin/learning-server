{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module DataBaseQueries where

import Types.User
import Instances.FromRow.User
import Data.Time.Clock
import Database.PostgreSQL.Simple

parseUsersList :: IO [(User)]
parseUsersList = do
    conn <- connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'"
    res <- query_ conn "SELECT name,login,password,createDate,isAdmin,isAbleToCreateNews FROM userslist"
    return res