module Chapter6.ExerciseSpec where

import Prelude hiding ((^), and, concat, replicate, (!!), elem)

import Test.Hspec
import Test.QuickCheck

import Chapter6.Code
import Chapter6.Exercise

spec :: Spec
spec = do     

  describe "Exercise 1: factorial with negative argument" $ do
    it "returns 0 when given negative argument" $  
      fac' (-1) `shouldBe` 0
 
  describe "Exercise 2: sumdown" $ do
    it "returns 0 when given 0" $
      sumdown 0 `shouldBe` 0
    it "returns 6 when given 3" $
      sumdown 3 `shouldBe` 6

  describe "Exercise 3 : ^" $ do
    it "returns 1 when given 2 and 0" $
      2 ^ 0  `shouldBe` 1
    it "returns 8 when given 2 and 3" $
      2 ^ 3 `shouldBe` 8
    it "returns 64 when given 2 and 6" $
      2 ^ 6 `shouldBe` 64

  describe "Exercise 4: euclid" $ do
    it "returns 3 when given 6 and 27" $
      euclid 6 27 `shouldBe` 3

  describe "Exercise 5" $ do
    describe "and" $ do
      it "returns True when given []" $
        and [] `shouldBe` True
      it "returns True when give all True's" $
        and [True,  True] `shouldBe` True
      it "returns False when one is False" $
        and [True, False] `shouldBe` False

    describe "concat" $ do
      it "flattens the nested list" $ do 
        concat [[1,2], [3, 4], [5]] `shouldMatchList` [1,2,3,4,5]
    
    describe "replicate" $ do
      it "returns an empty list when given 0" $
        replicate 0 'a' `shouldBe` ([]::[Char])
      it "returns a list of a specific count of the second argument" $ do
        replicate 3 'a' `shouldMatchList` ['a', 'a', 'a']
    
    describe "(!!)" $ do
      it "returns the head of the list when given 0" $
        [1,2,3] !! 0 `shouldBe` 1
      it "returns the element at specific index" $
        [1,2,3] !! 2 `shouldBe` 3

    describe "elem" $ do
      it "returns False if the list is empty" $
        'a' `elem` [] `shouldBe` False
      it "returns True when the list contains the element" $
        'c' `elem` ['a', 'b', 'c'] `shouldBe` True
      it "returns False when the list doesn't contain the element" $
        'd' `elem` ['a', 'b', 'c'] `shouldBe` False


  describe "Exercise 7: merge" $ do
    it "returns a sorted list when given two sorted lists" $
      merge [2,5,6] [1,3,4] `shouldMatchList` [1,2,3,4,5,6]

  describe "Exercise 8: msort" $ do
    it "returns a sorted list when givn an unsorted list" $
      do
          msort [2,5,6,1,3,4] `shouldMatchList` [1,2,3,4,5,6]
          msort [7,2,5,6,1,3,4] `shouldMatchList` [1,2,3,4,5,6,7]
