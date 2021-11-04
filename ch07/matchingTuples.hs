module TupleFunctions where

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (vw, wx) (xy, yz) = ((wx, yz), (vw, xy))

-- pattern matched: 

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- not pattern matched: 

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

-- Exercises Variety Pack pg 235


k (x,y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3, True)

--1a 
-- k :: (a, b) -> a
--1b 
-- k2 :: String
-- .. no. not same
--1c k1 and k3

-- 2

f' :: (a, b, c) -> (d, e, f)
  -> ((a, d), (c,f))
f' (a, b, c) (d, e, f) = ((a, d), (c,f))
