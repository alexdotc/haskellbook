-- Exercises Boxed vs Unboxed
module Main where

import Criterion.Main
import Data.Vector ((//))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

v :: V.Vector Int
v = V.fromList [1..100000]

uv :: UV.Vector Int
uv = UV.fromList [1..100000]

main :: IO ()
main = defaultMain
  [ bench "slicing vector" $ whnf (V.head . V.slice 100 99000) v
  , bench "slicing unboxed vector" $ whnf (UV.head . UV.slice 100 99000) uv
  ]
