module Types.Environment where

import Database.PostgreSQL.Simple (Connection)

data Environment = Environment 
    { dbConnection :: Connection
    }