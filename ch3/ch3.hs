-- Exercises: Scope

--  1. yes
--  2. no
--  3. no
--  4. yes

area d = pi * (r*r)
         where r = d/2

-- Exercises: Syntax Errors

--  1. no
--  2. no
--  3. yes

-- Chapter Exercsies

--  Reading Syntax 1 

--  a. correct
--  b. wrong, needs parens around prefixed ++
--  c. correct
--  d. wrong, missing endquote to close list
--  e. wrong, mismatched type
--  f. correct
--  g. wrong, only 1 argument given to 2 param func
--  h. correct

--  Reading Syntax 2

--  a. results in  [6,12,18]
--  b. results in "rainbow"
--  c. results in 10
--  d. results in "Jules"
--  e. results in [2,3,5,6,8,9]

--  Buidling Functions

fa :: String -> String
fa s = s ++ "!"

fb :: String -> String
fb s = take 1 (drop 4 s)

fc :: String -> String
fc s = drop 9 s

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex = ("Curry is awesome!"!!)

rvrs :: String -> String
rvrs s = drop 9 s ++ take 4 (drop 5 s) ++ take 5 s
