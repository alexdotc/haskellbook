module Data.Ini where

import Control.Applicative
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory
import Text.RawString.QQ
import Text.Trifecta

type Name = String
type Value = String
type Assignments = Map Name Value

newtype Header = Header String deriving (Eq, Ord, Show)

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  value <- some (noneOf "\n")
  skipEOL
  return (name, value)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany (do char ';' <|> char '#'
                            skipMany (noneOf "\n")
                            skipEOL)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

-- Exercise pg 1179
parseIniDir :: IO [FilePath] -> IO (Map FilePath (Result Config))
parseIniDir d = do
  fs <- d
  r <-(fmap . fmap) (\f -> parseString parseIni mempty f) (traverse readFile fs)
  return $ M.fromList $ zip fs r

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith p s@(_:xs)
  | length p > length s = False
  | length p == length s = p == s
  | otherwise = endsWith p xs

main :: IO ()
main = do
  let files = filter (\s -> endsWith ".ini" s) <$> (getCurrentDirectory >>= listDirectory)
  results <- parseIniDir files
  print results
-- End ch29 pg1179 exercise
