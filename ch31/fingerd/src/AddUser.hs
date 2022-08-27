{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import Database.SQLite.Simple.Types

import System.Environment

data User = User { userId :: Integer
                 , username :: Text
                 , shell :: Text
                 , homeDirectory :: Text
                 , realName :: Text
                 , phone :: Text
                 } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

type UserRow = (Null, Text, Text, Text, Text, Text)

addUser :: UserRow -> IO ()
addUser u = do
  conn <- open "finger.db"
  execute conn insertUser u
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  close conn

main :: IO ()
main = do
  a <- getArgs
  let ta = T.pack <$> a
  case (length a >= 5) of 
    True ->  addUser (Null, ta !! 0, ta !! 1, ta !! 2, ta !! 3, ta !! 4)
    False -> print "Please enter username, shell, homedir, fullname, phone"
