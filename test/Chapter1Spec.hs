
module Chapter1Spec where

import Test.Hspec

import Chapter1

spec :: Spec
spec = do
  describe "double" $ do
    it "returns double the value when given a number" $
      double 2 `shouldBe` 4

    it "returns double the value when given a negative number" $
       double (-2) `shouldBe` (-4)

    it "returns double the value when given a floating number" $
       double (2.5) `shouldBe` 5.0  
