--Chapter Exercises

import Control.Applicative
import Text.Trifecta

--1
--TODO

--2

parseDigit :: Parser Char
parseDigit = token (oneOf "1234567890")

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

--3

base10Integer' :: Parser Integer
base10Integer' = do
  m <- many (char '-')
  let b = base10Integer
  if null m then b else liftA2 (*) (pure (-1)) b

--4
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parseNDigit :: Int -> Parser Int
parseNDigit n = do
  m <- many $ char '('
  x <- read <$> count n parseDigit
  case null m of
    True -> return x
    False -> (char ')') >>= \_ -> return x

parsePhone :: Parser PhoneNumber
parsePhone = do
  optional $ string "1-" <|> string "1 "
  npa <- parseNDigit 3
  optional $ oneOf "- "
  exchange <- parseNDigit 3
  optional $ oneOf "- "
  line <- parseNDigit 4
  return $ PhoneNumber npa exchange line
    
    

    

--5
--TODO

--6
--TODO

--7
--TODO

--8
--TODO

--9
--TODO

--10
--TODO
