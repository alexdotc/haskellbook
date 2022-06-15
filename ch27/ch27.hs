{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}

import Debug.Trace (trace)

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"

hypo' :: IO ()
hypo' = do
  let x :: Int
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _    -> putStrLn "hello"

hypo'' :: IO ()
hypo'' = do
  let x :: Int
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"

-- Exercises Evaluate pg 1079

--1

-- const 1 undefined
-- (\a -> \b -> a) 1 undefined
-- (\b -> 1) undefined
-- 1

--2

-- const undefined 1
-- (\a -> \b -> a) undefined 1
-- (\b -> undefined) 1
-- bottom

--3

-- flip const undefined 1
-- (\b -> \a -> a) undefined 1
-- (\a -> a) 1
-- 1


--4

-- flip const 1 undefined
-- (\b -> \a -> a) 1 undefined
-- (\a -> a) undefined
-- bottom

--5

-- const undefined undefined
-- (\a -> \b -> a) undefined undefined
-- (\b -> undefined) undefined
-- bottom

--6

-- foldr const 'z' ['a'..'e']
-- const 'a' (foldr const 'z' ['b'..'e'])
-- 'a'

--7 

-- foldr (flip const) 'z' ['a'..'e']
-- (flip const) 'a' (foldr (flip const) 'z' ['b'..'e'])
-- (flip const) 'b' (foldr (flip const) 'z' ['c'..'e'])
-- (flip const) 'c' (foldr (flip const) 'z' ['d','e'])
-- (flip const) 'd' (foldr (flip const) 'z' ['e'])
-- (flip const) 'e' (foldr (flip const) 'z' [])
-- foldr (flip const) 'z' []
-- 'z'

inc = (+1)
twice = inc . inc

howManyTimes = inc (trace "I got eval'd" (1+1)) + twice (trace "I got eval'd" (1+1))

howManyTimes' = let onePlusOne = trace "I got eval'd" (1+1) in inc onePlusOne + twice onePlusOne

-- Chapter Exercises

-- What will :sprint output pg 1109

--1 x = 1
--  :spr x = _

--2 x = ['1']
--  :spr x = "1"

--3 x = [1]
--  :spr x = _ -- key here is to realize that it's not [_] because this is actually sugar for (1 :: Num a => a) : []
--  since Haskell is outside-in non-strict, the first argument to Cons (1) has a typeclass constraint and won't be opportunistically forced
--  to WHNF. 

--4 x = 1 :: Int
--  :spr x = 1

--5 f = \x -> x
--  x = f 1
--  :spr x = _ -- x is not in WHNF and won't be forced. x also won't shared once evaluated, since f is polymorphic

--6 f :: Int -> Int; f = \x -> x
--  x = f 1
--  :spr x = _ -- x is not in WHNF and won't be opportunistically forced by GHC, but will be shared once evaluated

-- Will printing this expression result in bottom? pg 1110

--1 snd (undefined, 1)
-- no

--2 let x = undefined
--  let y = x `seq` 1 in snd (x, y)
--  yes

--3 length $ [1..5] ++ undefined
-- yes

--4 length $ [1..5] ++ [undefined]
-- no

--5 const 1 undefined
-- no

--6 const 1 (undefined `seq` 1)
-- no

--7 const undefined 1
-- yes

-- Make the expression bottom pg 1110

x = undefined
y = "blah"
main = do
  print (snd (x, x `seq` y))
