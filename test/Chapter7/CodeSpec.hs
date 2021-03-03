module Chapter7.CodeSpec where

import Prelude hiding (map, filter)

import Test.Hspec
import Test.QuickCheck

import Chapter7.Code

spec :: Spec
spec = do
  describe "twice" $ do
    it "returns 12 when applied (*2) to 3" $ do
      twice (*2) 3 `shouldBe` 12

    it "returns the original list when applied reverse to a list" $ do
      property $ \xs -> twice reverse xs == (xs :: [Int])

  describe "map" $ do
    it "add 1 to each element" $ do
      map (+1) [1,3,5,7] `shouldMatchList` [2,4,6,8]
    it "check if each element is even" $ do
      map even [1,2,3,4] `shouldMatchList` [False,True,False,True]
    it "reverse each string" $ do
      map reverse ["abc", "def", "ghi"]  `shouldMatchList` ["cba", "fed", "ihg"]
    it "nested map" $ do
      map (map (+1)) [[1,2,3],[4,5]] `shouldMatchList` [[2,3,4],[5,6]]

  describe "filter" $ do
    it "returns a list of even numbers when applied to even" $ do
      filter even [1..10] `shouldMatchList` [2,4,6,8,10]
    it "returns a list of numbers larger than 5" $ do
      filter (> 5) [1..10] `shouldMatchList` [6,7,8,9,10]
    it "filters out blank spaces" $ do
      filter (/= ' ') "abc def ghi" `shouldBe` "abcdefghi"

  describe "sumsqreven" $ do
    it "returns the sum of square of even numbers" $
        sumsqreven [1,2,3,4,5,6] `shouldBe` 56