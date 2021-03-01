module Chapter1.CodeSpec where

import Prelude hiding (sum)

import Test.Hspec
import Test.QuickCheck

import Chapter1.Code

prop_double :: Int -> Bool
prop_double val = double val == 2 * val

spec :: Spec
spec = do
  describe "double" $ do
    it "returns double the value when given a positive number" $
      double 2 `shouldBe` 4

    it "returns double the value when given a negative number" $
       double (-2) `shouldBe` (-4)

    it "returns double the value when given a floating number" $
       double (2.5) `shouldBe` 5.0  

    it "should be same as 2*x" $ do
      property prop_double
       
  describe "sum" $ do
    it "returns 1 when given n = 1" $
      sum [1..1] `shouldBe` 1

    it "returns 55 when given n = 10" $
      sum [1..10] `shouldBe` 55

    it "returns 0 when given n = 0" $
      sum [1..0] `shouldBe` 0

  describe "quicksort" $ do
    it "return [] when given []" $
      qsort ([]::[Int]) `shouldMatchList` ([]::[Int])

    it "return [1] when given [1]" $
      qsort [1] `shouldMatchList` [1]

    it "return [1,2] when given [2,1]" $
      qsort [2,1] `shouldMatchList` [1,2]

    it "return [1,2,3,4,5] when given [3,5,1,4,2]" $
      qsort [3,5,1,4,2] `shouldMatchList` [1,2,3,4,5]
    

  describe "seqn" $ do
    it "return [] when given []" $
      seqn ([]::[IO Int]) `shouldReturn` ([]::[Int])