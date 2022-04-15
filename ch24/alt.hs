{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser (Either Integer String)
parseNos = (Left <$> integer) <|> (Right <$> some letter) 

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

main = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
  print $ p parseNos eitherOr
  print $ p (some parseNos) eitherOr
