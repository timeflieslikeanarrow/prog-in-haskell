module Chapter5.CodeSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter5.Code

spec :: Spec
spec = do
  describe "factors" $ do
    it "returns [1,3,5,15] when given 15" $
      factors 15 `shouldMatchList` [1,3,5,15]
    
    it "returns [1,7] when given 7" $
      factors 7 `shouldMatchList` [1,7]

  describe "prime" $ do
    it "returns False when given 15" $
      prime 15 `shouldBe` False

    it "returns True when given 7" $
      prime 7 `shouldBe` True

  describe "primes" $ do
    it "returns all prime numbers below 40 when given 40" $
      primes 40 `shouldMatchList` [2,3,5,7,11,13,17,19,23,29,31,37]

  describe "find" $ do
    it "returns all values for specific key" $
      find 'b' [('a', 1), ('b', 2), ('c', 3), ('b', 4)] `shouldMatchList` [2,4]

  describe "pairs" $ do
    it "returns all pairs of adjacent elements" $
      pairs [1,2,3,4] `shouldMatchList` [(1,2), (2,3), (3,4)]

  describe "isSorted" $ do
    it "returns True when given [1,2,3,4]" $
      isSorted [1,2,3,4] `shouldBe` True

    it "returns False when give [1,3,2,4]" $
      isSorted [1,3,2,4] `shouldBe` False

  describe "positions" $
    it "returns [1,3] when looking for False and given [True, False, True, False]" $
      positions False [True, False, True, False] `shouldMatchList` [1,3]

  describe "lowers" $
    it "returns 6 when given 'Haskell'" $
      lowers "Haskell" `shouldBe` 6

  describe "count" $
    it "returns 4 when looking for 's' in 'Mississippi'" $
      count 's' "Mississippi" `shouldBe` 4