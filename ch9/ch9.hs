-- Exercises EnumFromTo pg 307-308

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False,True]
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]

eftOrd LT LT = [LT]
eftOrd LT EQ = [LT,EQ]
eftOrd LT GT = [LT,EQ,GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ,GT]
eftOrd GT GT = [GT]
eftOrd GT _ = []

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b = []
  | a == b = [a]
  | a < b = a:(eftInt (succ a) b)

eftChar :: Char -> Char -> [Char]
eftChar a b 
  | a > b = []
  | a == b = [a]
  | a < b = a:(eftChar (succ a) b)

-- Exercises Thy Fearful Symmetry pg 311-312

-- 1
myWords :: String -> [String]
myWords [] = []
myWords s
  | length y == 0 = takeWhile (/= ' ') s : (myWords y)
  | otherwise = takeWhile (/= ' ') s : (myWords $ tail y)
  where y = dropWhile (/= ' ') s
