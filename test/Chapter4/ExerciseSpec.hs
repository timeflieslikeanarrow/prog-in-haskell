module Chapter4.ExerciseSpec where

import Prelude hiding ((&&),(||))

import Test.Hspec
import Test.QuickCheck

import Chapter4.Exercise

spec :: Spec
spec = do     

  describe "Exercise 1: halve" $ do
    it "returns a pair when given [1,2,3,4,5, 6]" $
      halve [1, 2, 3, 4, 5, 6] `shouldBe` ([1,2,3], [4,5,6])

  describe "Exercise 2: third" $ do
    it "returns third element of [1,2,3,4,5,6] using head/tail" $
      third [1, 2, 3, 4, 5, 6] `shouldBe` 3

    it "returns third element of [1,2,3,4,5,6] using index !!" $
      third' [1, 2, 3, 4, 5, 6] `shouldBe` 3

    it "returns third element of [1,2,3,4,5,6] using pattern matching" $
      third'' [1, 2, 3, 4, 5, 6] `shouldBe` 3

  describe "Exercise 3: safetail" $ do
    describe "using conditional expression" $ do
      it "returns the tail when given a non-empty list" $
        safetail [1,2] `shouldMatchList` [2]

      it "returns empty list when given an empty list" $
        safetail ([]::[Int])  `shouldMatchList` ([]::[Int])

    describe "using guarded equations" $ do
      it "returns the tail when given a non-empty list" $
        safetail' [1,2] `shouldMatchList` [2]
        
      it "returns empty list when given an empty list" $
        safetail' ([]::[Int])  `shouldMatchList` ([]::[Int])

    describe "using pattern matching" $ do
      it "returns the tail when given a non-empty list" $
        safetail'' [1,2] `shouldMatchList` [2]
        
      it "returns empty list when given an empty list" $
        safetail'' ([]::[Int])  `shouldMatchList` ([]::[Int])

  describe "Exercise 4: ||" $ do
     describe "use all 4 patterns" $ do
        it "True || True -> True" $
          True || True `shouldBe` True
        
        it "True || False -> True" $
          True || False `shouldBe` True

        it "False || True -> True" $
          False || True `shouldBe` True

        it "False || False -> False" $
          False || False `shouldBe` False

     describe "use 2 patterns" $ do
        it "True || True -> True" $
          True ||| True `shouldBe` True
        
        it "True || False -> True" $
          True ||| False `shouldBe` True

        it "False || True -> True" $
          False ||| True `shouldBe` True

        it "False || False -> False" $
          False ||| False `shouldBe` False

     describe "use 2 patterns with variable" $ do
        it "True || True -> True" $
          True |||| True `shouldBe` True
        
        it "True || False -> True" $
          True |||| False `shouldBe` True

        it "False || True -> True" $
          False |||| True `shouldBe` True

        it "False || False -> False" $
          False |||| False `shouldBe` False

     describe "use guarded equations" $ do
        it "True || True -> True" $
          True ||||| True `shouldBe` True
        
        it "True || False -> True" $
          True ||||| False `shouldBe` True

        it "False || True -> True" $
          False ||||| True `shouldBe` True

        it "False || False -> False" $
          False ||||| False `shouldBe` False

  describe "Exercise 5: &&" $ do
     describe "use nested conditional expressions" $ do
        it "True && True -> True" $
          True && True `shouldBe` True
        
        it "True && False -> False" $
          True && False `shouldBe` False

        it "False && True -> True" $
          False && True `shouldBe` False

        it "False && False -> False" $
          False && False `shouldBe` False  

  describe "Exercise 6: &&" $ do
     describe "use variables" $ do
        it "True && True -> True" $
          True &&& True `shouldBe` True
        
        it "True && False -> False" $
          True &&& False `shouldBe` False

        it "False && True -> True" $
          False &&& True `shouldBe` False

        it "False && False -> False" $
          False &&& False `shouldBe` False 

  describe "Exercise 8: Luhn algorithm" $ do
      describe "luhnDouble" $ do
        it "3 -> 6" $
          luhnDouble 3 `shouldBe` 6
        it "6 -> 3" $
          luhnDouble 6 `shouldBe` 3

      describe "luhn" $ do
        it "1 7 8 4 -> True" $ do
          luhn 1 7 8 4 `shouldBe` True
        it "4 7 8 3 -> False" $ do
          luhn 4 7 8 3 `shouldBe` False