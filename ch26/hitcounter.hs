{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Reader as R
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = 
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = case (M.lookup k m) of
  Nothing -> (M.insert k 1 m, 1)
  (Just v) -> (M.adjust (+1) k m, v+1)

-- TODO review this function for deeper understanding
app :: ScottyT Text (ReaderT Config IO) ()
app = get "/:key" $ do
  unprefixed <- param "key"
  counts' <- R.asks counts
  prefix' <- R.asks prefix
  let key' = mappend (prefix') unprefixed
  counts'' <- liftIO $ readIORef counts'
  let (newMap, newInteger) = bumpBoomp key' counts''
  liftIO $ writeIORef counts' newMap
  html $ mconcat ["<h1>Success! Count was: " 
          , TL.pack $ show newInteger
          , "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app
