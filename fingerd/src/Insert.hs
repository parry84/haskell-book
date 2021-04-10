{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad                (forever)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.List                    (intersperse)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple       hiding (close)
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket               as NS hiding (recv)
import           Network.Socket.ByteString    (recv, sendAll)
import           System.Environment
import           Text.RawString.QQ

import Lib

updateUser :: Query
updateUser = [r|
UPDATE
  users
SET
  username = :username,
  shell = :shell,
  homeDirectory = :homeDirectory,
  realName = :realName,
  phone = :phone
WHERE
  username = :username
|]

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    []     -> pure Nothing
    [user] -> pure $ Just user
    _      -> throwIO DuplicateData

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  case args of
    (username : shell : homeDirectory : realName : phone : _) -> do
      conn <- open "finger.db"
      maybeUser <- getUser conn (T.pack username)
      case maybeUser of
        Nothing ->
          execute conn insertUser ((Null, T.pack username, T.pack shell, T.pack homeDirectory, T.pack realName, T.pack phone) :: UserRow)
        Just user ->
          executeNamed conn updateUser [":username" := username, ":shell" := shell, ":homeDirectory" := homeDirectory, ":realName" := realName, ":phone" := phone]
      SQLite.close conn
    _ -> putStrLn "Wrong number of arguments"

