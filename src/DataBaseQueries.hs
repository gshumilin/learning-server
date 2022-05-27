{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module DataBaseQueries where

import Types.User
import Instances.FromRow.User
import Database.PostgreSQL.Simple
import Data.Int (Int64)

parseUsersList :: IO [(User)]
parseUsersList = do
    conn <- connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'"
    res <- query_ conn "SELECT name,login,password,createDate,isAdmin,isAbleToCreateNews FROM userslist"
    return res
    
writeUser :: User -> IO Int64
writeUser User {..} = do
    conn <- connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'"
    res <- execute conn "insert into userslist (name,login,password,createDate,isAdmin,isAbleToCreateNews) values (?,?,?,?,?,?)"
                    (name, login, password, createDate, isAdmin, isAbleToCreateNews)
    return res