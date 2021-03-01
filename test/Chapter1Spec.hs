module Chapter1Spec where

import Test.Hspec
import Test.QuickCheck

import Chapter1

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
       
