{-# LANGUAGE BangPatterns #-}

data List a = Nil | Cons a (List a) deriving Show

sTake :: Int -> List a -> List a
sTake n _
  | n <= 0 = Nil -- apparently you can mix guards and pattern matches and fall through like this. It probably desugars to the same thing.
sTake n Nil = Nil
sTake n (Cons x xs) = (Cons x (sTake (n-1) xs))

twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls

data List' a = Nil' | Cons' !a (List' a) deriving Show

sTake' :: Int -> List' a -> List' a
sTake' n _
  | n <= 0 = Nil'
sTake' n Nil' = Nil'
sTake' n (Cons' x xs) = (Cons' x (sTake' (n-1) xs))

twoEls' = Cons' 1 (Cons' undefined Nil')
oneEl' = sTake' 1 twoEls'

data List'' a = Nil'' | Cons'' !a (List'' a) deriving Show

sTake'' :: Int -> List'' a -> List'' a
sTake'' n _
  | n <= 0 = Nil''
sTake'' n Nil'' = Nil''
sTake'' n (Cons'' x !xs) = (Cons'' x (sTake'' (n-1) xs))

twoEls'' = Cons'' 1 (Cons'' undefined Nil'')
oneEl'' = sTake'' 1 twoEls''

threeElements = Cons'' 2 twoEls''
oneElT = sTake'' 1 threeElements

data List''' a = Nil''' | Cons''' !a !(List''' a) deriving Show

sTake''' :: Int -> List''' a -> List''' a
sTake''' n _
  | n <= 0 = Nil'''
sTake''' n Nil''' = Nil'''
sTake''' n (Cons''' x xs) = (Cons''' x (sTake''' (n-1) xs))

twoEls''' = Cons''' 1 (Cons''' undefined Nil''')
oneEl''' = sTake''' 1 twoEls'''
