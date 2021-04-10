{-# LANGUAGE OverloadedStrings #-}

module Lib(User(..), UserRow, DuplicateData(..), insertUser, getUserQuery) where

import           Control.Exception
import           Data.Text                    (Text)
import           Database.SQLite.Simple       hiding (close)
import           Database.SQLite.Simple.Types
import           Data.Typeable

data User = User {
  userId          :: Integer
  , username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) = toRow (id_, username, shell, homeDir, realName, phone)

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"
