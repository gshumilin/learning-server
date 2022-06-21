module DataBaseQueries.GetConnection where

import Database.PostgreSQL.Simple

getConnection :: IO Connection
getConnection = connectPostgreSQL "host='localhost' port=5432 dbname='learning_server_db' user='learning_server_user' password='pleasedonthackme'" 
