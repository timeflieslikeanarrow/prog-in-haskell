module Chapter7.ExerciseSpec where

import Prelude hiding (filter, map, all, any, takeWhile, dropWhile)

import Test.Hspec
import Test.QuickCheck

import Chapter7.Code 
import Chapter7.Exercise


spec :: Spec
spec = do     
  describe "Exercise 1: list comprehensions vs map/filter" $ do
    it "returs the same resultt" $ do
      property $ \xs -> ([x ^ 2 | x <- xs, even x] :: [Int]) == map (^2) (filter even xs)
 
  describe "Exercise 2" $ do
    describe "all" $ do
      it "returns True when given []" $ 
        all even [] `shouldBe` True
      it "returns True when given even [2,4,6]" $ 
        all even [2, 4, 6] `shouldBe` True
      it "returns False when given even [2,3,4]" $ 
        all even [2, 3, 4] `shouldBe` False
  
    describe "any" $ do
      it "returns False when given []" $ 
        any even [] `shouldBe` False
      it "returns False when given even [1,3,5]" $ 
        any even [1, 3, 5] `shouldBe` False
      it "returns True when given even [2,3,4]" $ 
        any even [2, 3, 4] `shouldBe` True

    describe "takeWhle" $ do
      it "returns [] when given []" $ 
        takeWhile even [] `shouldBe` []
      it "returns [] when given even [1,3,5]" $ 
        takeWhile even [1, 3, 5] `shouldBe` []
      it "returns [2] when given even [2,3,4]" $ 
        takeWhile even [2, 3, 4] `shouldBe` [2]

    describe "dropWhle" $ do
      it "returns [] when given []" $ 
        dropWhile even [] `shouldBe` []
      it "returns [1,3,5] when given even [1,3,5]" $ 
        dropWhile even [1, 3, 5] `shouldBe` [1, 3, 5]
      it "returns [3,4] when given even [2,3,4]" $ 
        dropWhile even [2, 3, 4] `shouldBe` [3,4]
      it "returns [] when given even [2,4,6]" $ 
        dropWhile even [2, 4, 6] `shouldBe` []

  describe "Exercise 3: map f/filter p with folder " $ do
    describe "map f" $ do
      it "map f vs foldr" $
        property $ \xs -> ((map even (xs::[Int]))::[Bool]) == foldr (\x s -> even x : s) [] xs

    describe "filter p" $ do
      it "filter f vs foldr" $
        property $ \xs -> ((filter even (xs::[Int]))::[Int]) == foldr (\x s -> if even x then (x : s) else s) [] xs


  describe "Exercise 4: dec2int" $ 
    it "returns 2345 when given [2,3,4,5]" $ do
      dec2int [2,3,4,5] `shouldBe` 2345

  describe "Exercise 6" $
    describe "map f" $ do
      it "map f vs unfold" $
        property $ \xs -> ((map even (xs::[Int]))::[Bool]) == ((map' even (xs::[Int]))::[Bool])
      it "iterate f vs unfold" $
        property $ \n -> take 10 (iterate (*2) (n::Int)) == take 10 (iterate' (*2) n)

  
  describe "Exercise 9" $
    it "altMap" $ do
      altMap (+10) (+100) [0,1,2,3,4] `shouldMatchList` [10, 101, 12, 103, 14]
  
  describe "Exercise 10: luhn redo" $ do
    it "valid card" $ do
      luhn [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1] `shouldBe` True
    it "invalid card" $ do
      luhn [4,0,1,2,8,8,8,8,8,8,8,8,1,8,9,1] `shouldBe` False