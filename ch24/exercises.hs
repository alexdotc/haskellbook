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
  m <- many (oneOf "-")
  let b = base10Integer
  if null m then b else liftA2 (*) (pure (-1)) b

--4
--TODO

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
