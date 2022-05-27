{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module Instances.FromRow.User where

import Types.User
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field