module Chapter5.ExerciseSpec where

import Prelude hiding ((&&),(||))

import Test.Hspec
import Test.QuickCheck

import Chapter5.Exercise

spec :: Spec
spec = do     

  describe "Exercise 1: sum of squares" $ do
    --https://www.wolframalpha.com/input/?i=sum+of+squares+from+1+to+100
    it "returns 338350 when given 100" $  
      sumOfSquares 100 `shouldBe` 338350
  
  describe "Exercise 2: grid" $ do
    it "a grid of coordinates [0..m] [0..n]" $
      grid 1 2 `shouldMatchList` [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]

  describe "Exercise 3: coordinate squares" $ do
    it "excluding diagonals" $
      square 2 `shouldMatchList` [(0, 1), (0,2), (1, 0), (1, 2), (2, 0), (2,1)]

  describe "Exercise 4: replicate" $ do
    it "return [True, True, True] when given 3 and True" $
      replicate' 3 True `shouldMatchList` [True, True, True]

  describe "Exercise 5: pyths" $ do
    it "returns [(3,4,5), (4,3,5), (6,8,10), (8,6,10)] when given 10" $
      pyths 10 `shouldMatchList` [(3,4,5), (4,3,5), (6,8,10), (8,6,10)]

  describe "Exercise 6: perfect numbers" $ do
    it "returns [6,28,496] when given 500" $
      perfects 500 `shouldMatchList` [6,28,496]

  describe "Exercise 9: Scala product" $ do
    it "returns 32 when given [1,2,3] and [4,5,6]" $
      scalarproduct [1,2,3] [4,5,6] `shouldBe` 32
