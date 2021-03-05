module Chapter11.ExerciseSpec where

import Data.List

import Test.Hspec
import Test.QuickCheck

import Chapter11.TicTacToe
import Chapter11.Exercise

spec :: Spec
spec = do
  
  --Exercise 1
  describe "count nodes" $ do
    it "returns 549946" $
      countGameTreeNode (gametree empty O) `shouldBe` 549946

  describe "count depth" $ do
    it "returns 9" $
      maxDepth (gametree empty O) `shouldBe` 9