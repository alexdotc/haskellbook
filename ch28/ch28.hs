module Main where

import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as E
import Data.Vector ((//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

m2 :: M.Map Int Int
m2 = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (10000, 10000)


s2 :: S.Set Int
s2 = S.fromList $ take 10000 stream
  where stream = iterate (+1) 10000

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertMap :: Int -> M.Map Int Int 
insertMap i = M.insert i i m

insertSet :: Int -> S.Set Int 
insertSet i = S.insert i s

unionMap :: M.Map Int Int -> M.Map Int Int
unionMap m2 = M.union m m2

unionSet :: S.Set Int -> S.Set Int
unionSet s2 = S.union s s2

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [E.Seq Int]
seqs = replicate 10 (E.fromList [1..100000])

list :: [Int]
list = [1..100000]

seq :: E.Seq Int
seq = E.fromList [1..100000]

slice :: Int -> Int -> [a] -> [a]
slice from len xs = take len (drop from xs)

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
  where go 0 v = v
        go n v = go (n-1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
  where updates = (\n -> (n,0)) <$> [1..n]

batchVec :: Int -> V.Vector Int
batchVec n = V.unsafeUpdate vec updates
  where updates = (\n -> (n,0)) <$> (V.fromList [1..n])

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = return v
        go n v = (MV.write v n 0) >> go (n-1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec
  where
    go 0 v = V.freeze v
    go n v = (MV.write v n 0) >> go (n-1) v

main :: IO ()
main = defaultMain
  [ bench "member check map" $ whnf membersMap 9999
  , bench "member check set" $ whnf membersSet 9999
  , bench "concat lists" $ nf mconcat lists
  , bench "concat seqs" $ nf mconcat seqs
  , bench "indexing lists" $ whnf (\xs -> xs !! 9001) list
  , bench "indexing seqs" $ whnf (flip E.index 9001) Main.seq
  , bench "slicing list" $ whnf (head . slice 100 900) l
  , bench "slicing vector" $ whnf (V.head . V.slice 100 900) v
  , bench "slow vector update" $ whnf slow 9999
  , bench "fast vector update" $ whnf batchList 9999
  , bench "extra fast vector update" $ whnf batchVec 9999
  , bench "mutable IO vector update" $ whnfIO (mutableUpdateIO 9998)
  , bench "mutable ST vector update" $ whnf mutableUpdateST 9998
--  Exercise Benchmark Practice pg 1136
  , bench "insert map" $ whnf insertMap 10000
  , bench "insert set" $ whnf insertSet 10000
  , bench "union map" $ nf unionMap m2
  , bench "union set" $ nf unionSet s2
  ]

-- Chapter Exercises
-- DList pg 1156

newtype DList a = DL { unDL :: [a] -> [a] }

--1
empty :: DList a
{-# INLINE empty #-}
empty = DL $ \_ -> []

--2
singleton :: a -> DList a
{-# INLINE singleton #-}
singleton = \x -> (DL $ \_ -> [x])

--3
toList :: DList a -> [a]
{-# INLINE toList #-}
toList dl = unDL dl []
