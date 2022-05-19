{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
