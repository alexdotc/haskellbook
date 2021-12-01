module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 :: Integer) + (1 :: Integer)) > (1 :: Integer) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 :: Integer) + (2 :: Integer) `shouldBe` 4
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
--Intermission: Short Exercises
  describe "Multiplication" $ do
    it "25 times 30 is 750" $ do
      rmult 25 30 `shouldBe` 750
    it "43 times 71 is 3053" $ do
      rmult 43 71 `shouldBe` 3053

rmult :: Integral a => a -> a -> a
rmult a 1 = a
rmult a b = a + rmult a (b-1)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
              go (n-d) d (count+1)
