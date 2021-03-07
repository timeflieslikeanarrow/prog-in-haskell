module Chapter14.CodeSpec where

import Data.Monoid
import Data.Foldable
--import Data.Traversal

import Test.Hspec
import Test.QuickCheck

import Chapter14.Code

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

spec :: Spec
spec = do
  describe "Monoid" $ do
    describe "Sum" $ do
      it "2 + 3 + 4" $  
        mconcat [Sum 2, Sum 3, Sum 4] `shouldBe` Sum 9
    
    describe "Product" $ do
      it "2 * 3 * 4" $  
        mconcat [Product 2, Product 3, Product 4] `shouldBe` Product 24

    describe "All" $ do
      it "True, True, True " $  
        mconcat [All True, All True, All True] `shouldBe` All True

      it "True, False, True " $  
        mconcat [All True, All False, All True] `shouldBe` All False
    
    describe "Any" $ do
      it "False, False, False " $  
        mconcat [Any False, Any False, Any False] `shouldBe` Any False

      it "False, True, False " $  
        mconcat [Any False, Any True, Any False] `shouldBe` Any True
  
  describe "Foldable" $ do
      describe "foldMap" $ do
        it "Sum" $ 
          getSum (foldMap Sum [1..10]) `shouldBe` 55
        
        it "Product" $
          getProduct (foldMap Product [1..10]) `shouldBe` 3628800

      describe "Tree" $ do
        it "foldr" $
          foldr (+) 0 tree `shouldBe` 6

        it "foldl" $
          foldl (+) 0 tree `shouldBe` 6

      describe "null" $ do
        it "[]" $ 
          null [] `shouldBe` True

        it "Leaf 1" $
          null (Leaf 1) `shouldBe` False
      
      describe "length" $ do
        it "[1..10]" $
          length [1..10] `shouldBe` 10

        it "Tree" $ do
          length (Node (Leaf 'a') (Leaf 'b')) `shouldBe` 2
          length tree `shouldBe` 3

      describe "foldr1" $ do
        it "[1..10]" $
          foldr1 (+) [1..10] `shouldBe` 55

        it "Tree" $ do
          foldr1 (+) (Node (Leaf 1) (Leaf 2)) `shouldBe` 3
          foldr1 (+) tree `shouldBe` 6

      describe "toList" $ do
        it "Tree" $ do
          toList tree `shouldMatchList` [1,2,3]

      describe "average" $ do
        it "[1..10]" $
          average [1..10] `shouldBe` 5

        it "Tree" $
          average (Node (Leaf 1) (Leaf 3)) `shouldBe` 2

      describe "and" $ do
        it "[]" $ do
          and [True, False, True] `shouldBe` False
          and [True, True, True] `shouldBe` True
        
      describe "or" $ do
        it "Tree" $ 
          or (Node (Leaf True) (Leaf False)) `shouldBe` True

      describe "all" $ do
        it "even []" $
          all even [1,2,3] `shouldBe` False
      
      describe "any" $ do
        it "even Tree" $
          any even (Node (Leaf 1) (Leaf 2)) `shouldBe` True
          

      describe "concat" $ do
        it "[]" $
          concat ["ab", "cd", "ef"] `shouldBe` "abcdef"
        
        it "Tree" $
          concat (Node (Leaf [1,2]) (Leaf [3])) `shouldMatchList` [1,2,3]

  describe "Traversal" $ do
      describe "dec" $ do
        it "[]" $ do
          traverse dec [1,2,3] `shouldBe` Just [0,1,2]
          traverse dec [2,1,0] `shouldBe` Nothing

        it "Tree" $ do
          traverse dec (Node (Leaf 1) (Leaf 2)) `shouldBe` Just (Node (Leaf 0) (Leaf 1))
          traverse dec (Node (Leaf 0) (Leaf 1)) `shouldBe` Nothing

      describe "sequenceA" $ do
        it "[Maybe]" $ do
          sequenceA [Just 1, Just 2, Just 3] `shouldBe` Just [1,2,3]
          sequenceA [Just 1, Nothing, Just 3] `shouldBe` Nothing
        
        it "Tree" $ do
          sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2))) `shouldBe` Just (Node (Leaf 1) (Leaf 2))
          sequenceA (Node (Leaf (Just 1)) (Leaf Nothing)) `shouldBe` Nothing