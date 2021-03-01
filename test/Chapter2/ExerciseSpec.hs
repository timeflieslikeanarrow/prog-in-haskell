module Chapter2.ExerciseSpec where

import Prelude hiding (last, init)

import Test.Hspec
import Test.QuickCheck

import Chapter2.Code

spec :: Spec
spec = do     

  describe "last" $ do
    it "returns 5 when given [1,2,3,4,5]" $
      last [1, 2, 3, 4, 5] `shouldBe` 5

  describe "init" $ do
    it "returns [1,2,3,4] when given [1,2,3,4,5]" $
      init [1, 2, 3, 4, 5] `shouldMatchList` [1,2,3,4]