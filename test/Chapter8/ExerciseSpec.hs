module Chapter8.ExerciseSpec where

import Prelude hiding (filter, map, all, any, takeWhile, dropWhile)

import Test.Hspec
import Test.QuickCheck

import Chapter8.Code 
import Chapter8.Exercise

spec :: Spec
spec = do     
  describe "Exercise 1: add/mult for Nat" $ do
    it "add 2 + 1 == 3" $ 
      nat2int (add (Succ (Succ Zero)) (Succ Zero)) `shouldBe` 3
    it "mult 3 * 2 == 6" $
      nat2int (mult (Succ (Succ (Succ Zero))) (Succ (Succ Zero))) `shouldBe` 6

  describe "Exercise 2: Ordering" $ do
    it "6 in t" $
      occurs' 6 t `shouldBe` True
    it "8 not in t" $
      occurs' 8 t `shouldBe` False
    it "6 in t using compare" $
      occurs'' 6 t `shouldBe` True
    it "8 not in t using compare" $
      occurs'' 8 t `shouldBe` False

  describe "Exercise 3: balanced" $ do
    it "t2 is balanced" $
      balanced t2 `shouldBe` True
  
    it "t3 is not balanced" $
      balanced t3 `shouldBe` False

  describe "Exercise 4: balance" $ do
    it "trees generated by balance are always balanced" $
      property $ \xs -> length xs >= 2 ==> balanced (balance (xs::[Int])) == True
