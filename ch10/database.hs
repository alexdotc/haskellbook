-- Exercises Database Processing 371-373

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 8099
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate (DbDate t:xs) = [t] ++ filterDbDate xs
filterDbDate (x:xs) = filterDbDate xs
filterDbDate [] = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber (DbNumber i:xs) = [i] ++ filterDbNumber xs
filterDbNumber (x:xs) = filterDbNumber xs
filterDbNumber [] = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 $ filterDbNumber xs

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb $ xs) / 
            (fromIntegral . length . filterDbNumber $ xs)

main :: IO ()
main = do
    print(filterDbNumber theDatabase)
