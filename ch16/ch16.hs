--Exercises Be Kind pg 634

--1 *
--2 both are * -> *
--3 * -> * -> *

-- double fmap pg 650


-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- fmap :: Functor g => (x -> y) -> g x -> g y
-- fmap :: Functor f => (m -> n) -> f m -> f n
--
-- UNCURRY
--
-- (.) :: (Functor f, Functor g) => ((x -> y) -> (g x -> g y))
--                                  -> ((m -> n) -> (f m -> f n)) 
--                                  -> ((m -> n) -> (g x -> g y))
--
-- REPLACE left fmap's (x -> y) with b from right fmap in (.)
--
-- (.) :: (Functor f, Functor g) => ((f m -> f n) -> (g (f m) -> g (f n)))
--                                  -> ((m -> n) -> (f m -> f n)) 
--                                  -> (m -> n) -> (g (f m) -> g (f n))
--
-- APPLY (.)
--
-- (fmap . fmap) :: (Functor f, Functor g) => (m -> n) -> g (f m) -> g (f n)

-- Exercises Heavy Lifting pg 656

--1 
a = fmap (+1) $ read "[1]" :: [Int]

--2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--3
c = fmap (*2) (\x -> x - 2)

--4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

--5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
--      changed = fmap read (fmap ("123" ++) (fmap show ioi))
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed

-- Exercises Instances of Func pg 663
--
-- No, Trivial is a type constant with kind * ( not * -> * )
