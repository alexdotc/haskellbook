import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse'' :: Parser String -> IO ()
testParse'' p = print $ parseString p mempty "123"

testParse' :: Parser () -> IO ()
testParse' p = print $ parseString p mempty "12"

pNL s = putStrLn('\n' : s)

--Exercises Parsing Practice

--1
oneEOF = one >> eof
oneTwoEOF = oneTwo >> eof

--2
oneTwoThree :: String -> Parser String
oneTwoThree = \s -> case s of
                         "1"   -> string "1"
                         "12"  -> string "12"
                         "123" -> string "123"
                         _     -> stop

--3
pStr :: String -> Parser String
pStr [] = return []
pStr (x:xs) = char x >>= (\px -> pStr xs >>= (\ps -> return (px:ps)))

--Exercises Unit of Success
pInt :: Parser Integer
pInt =  integer >>= (\pn -> eof >> return pn)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "oneEOF:"
  testParse' oneEOF
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwoEOF:"
  testParse' oneTwoEOF
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoThree:"
  testParse'' $ oneTwoThree "1"
  testParse'' $ oneTwoThree "12"
  testParse'' $ oneTwoThree "123"
