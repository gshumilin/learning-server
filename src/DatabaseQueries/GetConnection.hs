module DatabaseQueries.GetConnection where

import Database.PostgreSQL.Simple

getConnection :: IO Connection
getConnection = connectPostgreSQL "host='localhost' port=5432 dbname='server' user='server_user' password='12345'" 