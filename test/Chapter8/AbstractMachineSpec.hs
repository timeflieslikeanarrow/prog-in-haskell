module Chapter8.AbstractMachineSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter8.AbstractMachine

e1 :: Expr  -- (2+3) + 4 == 9
e1 = Add (Add (Val 2) (Val 3)) (Val 4)

e2 :: Expr -- (2*3) + (4*5) == 26
e2 = Add (Mul (Val 2) (Val 3))
         (Mul (Val 4) (Val 5))

e3 :: Expr -- (2*(3+2)) + (4*(5+2*4) == 62
e3 = Add (Mul (Val 2) (Add (Val 3) (Val 2)))
         (Mul (Val 4) (Add (Val 5) (Mul (Val 2) (Val 4))))

spec :: Spec
spec = do     
  describe "value" $ do
    it "adding (2 + 3) + 4 returns 9" $
      value e1 `shouldBe` 9

  describe "Exercise 6" $ do
    describe "size" $ do  
      it "e1 has 3 values" $ 
        size e1 `shouldBe` 3

      it "e2 has 4 values" $ 
        size e2 `shouldBe` 4

    describe "eval'" $ do
      it "e1 -> 9" $ do
        eval e1 [] `shouldBe` 9
        eval' e1 `shouldBe` 9
  
  describe "Exercise 9" $ do
    it "e2 -> 26" $ do
      eval e2 [] `shouldBe` 26
      eval' e2 `shouldBe` 26
    it "e3 -> 62" $ do
      eval e3 [] `shouldBe` 62
      eval' e3 `shouldBe` 62