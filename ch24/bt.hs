{-# LANGUAGE OverloadedStrings #-}

module BT where

import Control.Applicative
import Data.Attoparsec.ByteString as A hiding (try, parseTest, (<?>))
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a => Text.Trifecta.Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  trifP nobackParse "12"
  trifP tryParse "12"
  trifP tryAnnot "13"
  parsecP nobackParse "3"
  parsecP tryParse "12"
  attoP nobackParse "3"
  attoP tryParse "12"
