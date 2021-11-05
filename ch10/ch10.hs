1-- Exercises Understanding Folds pg 365-367

-- 1 b and c
-- 2 
-- NOTE: I did this for [1..5] instead of [1..3] by accident but same idea just longer and more annoying
-- foldl (flip (*)) 1 [1..5]
-- = foldl (flip (*)) (1*1) [2..5]
-- = foldl (flip (*)) (2*(1*1)) [3..5]
-- = foldl (flip (*)) (3*(2*(1*1))) [4..5]
-- = foldl (flip (*)) (4*(3*(2*(1*1)))) [5]
-- = foldl (flip (*)) (5*(4*(3*(2*(1*1))))) []
-- = (5*(4*(3*(2*(1*1)))))
-- = (5*(4*(3*(2*1))))
-- = (5*(4*(3*2)))
-- = (5*(4*6))
-- = (5*24)
-- = 120
--
-- 3 c
-- 4 a
-- 5a foldr (++) [] ["woot", "WOOT", "woot"] -- no base
-- 5b foldr max '\0' "fear is the little death" -- maybe there's a better more haskell-y way than null char?
-- 5c foldr (&&) True [False, True]
-- 5d foldr (||) False [False, True]
-- 5e foldr ((++) . show) "" [1..5]
-- 5f foldr (flip const) 'a' [1..5] -- this needs to return char since our base is 'a'
-- 5g foldr (flip const) 0 'tacos'
-- 5h foldl const 0 "burritos"
-- 5i foldl const 'z' [1..5]


