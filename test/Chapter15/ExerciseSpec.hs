module Chapter15.ExerciseSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter15.Exercise

spec :: Spec
spec = do

  describe "Exercise 4: fibs" $ do
    it "first 10" $  
     take 10 fibs `shouldMatchList` [0,1,1,2,3,5,8,13,21,34]
  
  describe "Exercise 6: sqroot" $ do
    it "for 2.0" $
      abs (sqroot 2.0 - 1.414213) < 0.00001 `shouldBe` True

    it "for 4.0" $
      abs (sqroot 4.0 - 2.0) < 0.00001 `shouldBe` True