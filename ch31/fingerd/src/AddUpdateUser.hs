{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.Typeable
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
  toRow (User id_ username shell homeDirectory realName phone) =
    toRow (id_, username, shell, homeDirectory, realName, phone)

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

-- 1 except just do it in sqlite3 with hardcoded values after ".open finger.db"
updateUserQuery :: Query
updateUserQuery = "UPDATE users SET shell = ?, homeDirectory = ?, realName = ?, phone = ? WHERE username = ?"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <-
    query conn getUserQuery (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

--2
insertUser :: UserRow -> Connection -> IO ()
insertUser u c = do
  execute c insertUserQuery u

--3
updateUser :: UserRow -> Connection -> IO ()
updateUser (_,u,s,h,r,p) c = do
  execute c updateUserQuery (s,h,r,p,u)

addOrUpdateUser :: UserRow -> IO ()
addOrUpdateUser u@(_,s,_,_,_,_) = do
  conn <- open "finger.db"
  exists <- getUser conn s
  case exists of
    Nothing -> insertUser u conn
    _       -> updateUser u conn
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  close conn
  
main :: IO ()
main = do
  a <- getArgs
  let ta = T.pack <$> a
  case (length a >= 5) of 
    True ->  addOrUpdateUser (Null, ta !! 0, ta !! 1, ta !! 2, ta !! 3, ta !! 4)
    False -> print "Please enter username, shell, homedir, fullname, phone"
