module Chapter6.CodeSpec where

import Prelude hiding ((*), product, length, reverse, (++), zip, drop, even, odd)

import Test.Hspec
import Test.QuickCheck

import Chapter6.Code

spec :: Spec
spec = do
  describe "fac" $ do
    it "returns 24 when given 4" $ do
      fac 4 `shouldBe` 24
      fac' 4 `shouldBe` 24

  describe "*" $ do
    it "returns 12 when given 4 3" $
      4 * 3 `shouldBe` 12

  describe "product" $ do
    it "returns 24 when given [2,3,4]" $
      product [2,3,4] `shouldBe` 24

  describe "length" $ do
    it "returns 3 when give [2,3,4]" $
      length [2,3,4] `shouldBe` 3

  describe "reverse" $ do
    it "returns [3,2,1] when given [1,2,3]" $
      reverse [1,2,3] `shouldMatchList` [3,2,1]
    
  describe "++" $ do
    it "returns [1,2,3,4,5] when given [1,2,3] and [4,5]" $
      [1,2,3] ++ [4,5] `shouldMatchList` [1,2,3,4,5]

  describe "insert" $ do
    it "returns [1,2,3,4,5] when given 3 an [1,2,4,5]" $
      insert 3 [1,2,4,5] `shouldMatchList` [1,2,3,4,5]

  describe "isort" $ do
    it "returns [1,2,3,4] when given [3,2,1,4]" $ 
      isort [3,2,1,4] `shouldMatchList` [1,2,3,4]
  
  describe "zip" $ do
    it "returns [('a', 1), ('b', 2), ('c', 3)] when given ['a', 'b', 'c'] and [1,2,3,4]" $
      zip ['a', 'b', 'c'] [1,2,3,4] `shouldMatchList` [('a', 1), ('b', 2), ('c', 3)]

  describe "drop" $ do
    it "returns [] when given []" $
      drop 3 ([]::[Int]) `shouldBe` []
    it "returns [1,2,3,4] when given 0 and [1,2,3,4]" $
      drop 0 [1,2,3,4] `shouldBe` [1,2,3,4]
    it "returns [2,3,4] when given 1 and [1,2,3,4]" $
      drop 1 [1,2,3,4] `shouldMatchList` [2,3,4]

  describe "fib" $ do
    it "returns 0 when given 0" $
      fib 0 `shouldBe` 0
    it "returns 1 when given 1" $
      fib 1 `shouldBe` 1
    it "returns 1 when given 2" $
      fib 2 `shouldBe` 1
    it "returns 2 when given 3" $
      fib 3 `shouldBe` 2
    it "returns 3 when given 4" $
      fib 4 `shouldBe` 3
    it "returns 5 when given 5" $
      fib 5 `shouldBe` 5
    it "returns 8 when given 6" $
      fib 6 `shouldBe` 8

  describe "qsort" $ do
    it "returns [1,2,3,4] when given [1,4,3,2]" $
      qsort [1,4,3,2] `shouldMatchList` [1,2,3,4]

  describe "even/odd mutual recursion" $ do
    it "returns False when even is given 7" $
      even 7  `shouldBe` False
    it "returns True when odd is given 7" $
      odd 7 `shouldBe` True
    it "returns True when even is given 4" $
      even 4  `shouldBe` True
    it "returns False when odd is given 4" $
      odd 4  `shouldBe` False
  
  describe "evens/odds" $ do
    it "returns 'ace' when evens is given 'abcde'" $
      evens "abcde" `shouldBe` "ace"
    it "returns 'bd' when odds is given 'abcde'" $
      odds "abcde" `shouldBe` "bd"