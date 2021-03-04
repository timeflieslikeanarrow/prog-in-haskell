module Chapter9.CodeSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter9.Code

numbers :: [Int]
numbers = [1,3,7,10,25,50]

e :: Expr
e = App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 1) (Val 50))

spec :: Spec
spec = do
  describe "valid" $ do
    it "add is always valid" $ do
      property $ \x y -> x > 0 && y > 0 && x <= y ==> valid Add x y == True
    
    describe "sub" $ do
      it "valid when left operand > right operand" $ do
        property $ \x y -> x > 0 && y > 0 && x > y ==> valid Sub x y == True
    
      it "invalid when left operand <= right operand" $ do
        valid Sub 2 3 `shouldBe` False

    it "mul is alawys valid" $ do
      property $ \x y -> x > 0 && y > 0 && x /= 1 && y /= 1 && x <= y ==> valid Mul x y == True

    it "div is valid when left operand can evenly divide right operand" $ do
      --property $ \x y -> x > 0 && y > 0 && y /= 1 && x >= y && (x `mod` y == 0) ==> valid Div x y == True
      valid Div 10 2 `shouldBe` True
      valid Div 10 3 `shouldBe` False

  describe "expr" $ do
    it "1+(2*3)" $
      show (App Add (Val 1) (App Mul (Val 2) (Val 3))) `shouldBe` "1+(2*3)"

  describe "values" $ do
    it "1+(2*3) -> [1,2,3]" $
     values (App Add (Val 1) (App Mul (Val 2) (Val 3))) `shouldMatchList` [1,2,3]

  describe "eval" $ do
    it "2+3 -> [5]" $
      eval (App Add (Val 2) (Val 3)) `shouldBe` [5]
    it "2-3 -> []" $
      eval (App Sub (Val 2) (Val 3)) `shouldBe` []

  describe "subs" $ do
    it "[1]" $
      subs [1] `shouldMatchList` [[], [1]]
    it "[1,2]" $
      subs [1,2] `shouldMatchList` [[], [2],[1],[1,2]]
    it "[1,2,3]" $
      subs [1,2,3] `shouldMatchList` [[], [3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

  describe "interleave" $ do
    it "1 [2,3,4]" $
      interleave 1 [2,3,4] `shouldMatchList` [[1,2,3,4], [2,1,3,4], [2,3,1,4], [2,3,4,1]]

  describe "perms" $ do
    it "[1,2,3]" $
      perms [1,2,3] `shouldMatchList` [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]]

  describe "choices" $ do
    it "[1,2,3]" $
      choices [1,2,3] `shouldMatchList` [[], [3], [2], [2, 3], [3, 2], [1], [1, 3], [3, 1], [1, 2], [2, 1], 
                                         [1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]]

  describe "solution" $ do
    it "e 765" $
      solution e [1,3,7,10,25,50] 765 `shouldBe` True

  describe "split" $ do
    it "[4]" $
      split [4] `shouldMatchList` []
    it "[3,4]" $
      split [3,4] `shouldMatchList` [([3],[4])]
    it "[2,3,4]" $
      split [2,3,4] `shouldMatchList` [([2],[3,4]), ([2,3],[4])]
    it "[1,2,3,4]" $
      split [1,2,3,4] `shouldMatchList` [([1], [2,3,4]), ([1,2],[3,4]), ([1,2,3], [4])]

  describe "solution'" $ do
    it "780 solutions for 765" $
      length (solutions' [1,3,7,10,25,50] 765) `shouldBe` 49 -- before: 780