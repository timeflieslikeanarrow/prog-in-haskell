module Chapter1.ExerciseSpec where

import Prelude hiding (sum, product)

import Test.Hspec
import Test.QuickCheck

import Chapter1.Code

--Exercise 2
sum_singleton :: Int -> Bool
sum_singleton n = sum [n] == n

spec :: Spec
spec = do       
  describe "sum" $ do
    it "returns n when given [n]" $ 
      property sum_singleton

  describe "product" $ do
    it "returns 24 when given [2,3,4]" $
      product [2,3,4] `shouldBe` 24

  describe "quicksort in descending order" $ do
    it "return [] when given []" $
      qsort' ([]::[Int]) `shouldMatchList` ([]::[Int])

    it "return [1] when given [1]" $
      qsort' [1] `shouldMatchList` [1]

    it "return [2,1] when given [2,1]" $
      qsort' [2,1] `shouldMatchList` [2,1]

    it "return [5,4,3,2,1] when given [3,5,1,4,2]" $
      qsort' [3,5,1,4,2] `shouldMatchList` [5,4,3,2,1]
    
    it "return [5,4,3,2,1] when given [1,2,3,4,5]" $
      qsort' [1,2,3,4,5] `shouldMatchList` [5,4,3,2,1]

  describe "seqn" $ do
    it "return [] when given []" $
      seqn ([]::[IO Int]) `shouldReturn` ([]::[Int])