{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent           (forkIO, threadDelay)
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
import           Text.RawString.QQ

import           Lib

createUsers :: Query
createUsers = [r|
  CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  shell TEXT,
  homeDirectory TEXT,
  realName TEXT,
  phone TEXT)
|]

allUsers :: Query
allUsers =
  "SELECT * from users"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    []     -> pure Nothing
    [user] -> pure $ Just user
    _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
    where
      meRow :: UserRow
      meRow = ( Null
              , "callen"
              , "/bin/zsh"
              , "/home/callen"
              , "Chris Allen"
              , "555-123-4567"
              )

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames       = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  [ "Login: ",      e username, "\t\t\t\t"
  , "Name: ",       e realName, "\n"
  , "Directory: " , e homeDir,  "\t\t\t"
  , "Shell :",      e shell,    "\n"
  ]
    where
      e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing   -> do
      putStrLn $ "Couldn't find matching user for username: " ++ show username
      pure ()
    Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name   -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  NS.close soc

readUser :: Connection -> Socket -> IO ()
readUser conn sock = forever $ do
  (soc, _) <- accept sock
  msg <- recv soc 1024
  let userData = BS.split (fromIntegral $ fromEnum ' ') msg
  sendAll soc $ userData!!0
  execute conn insertUser ((
        Null
        , decodeUtf8 $ userData!!0
        , decodeUtf8 $ userData!!1
        , decodeUtf8 $ userData!!2
        , decodeUtf8 $ userData!!3
        , decodeUtf8 $ userData!!4
        ) :: UserRow)
  NS.close soc

fingerd :: IO ()
fingerd = withSocketsDo $ do
  addrinfos <- getAddrInfo
               (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
               Nothing (Just "30079")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  NS.bind sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  NS.close sock

addUsers :: IO ()
addUsers = withSocketsDo $ do
  addrinfos <- getAddrInfo
               (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
               Nothing (Just "30080")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  NS.bind sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  readUser conn sock
  SQLite.close conn
  NS.close sock

main :: IO ()
main = do
  forkIO fingerd
  forkIO addUsers
  forever $ threadDelay 1000000
  pure ()
