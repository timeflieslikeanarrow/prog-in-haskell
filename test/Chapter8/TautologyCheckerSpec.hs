module Chapter8.TautologyCheckerSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter8.TautologyChecker

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A')) -- A ^ ~A

p2 :: Prop 
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')  -- (A ^ B) => A

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))  -- A => (A ^ B)

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')  -- (A ^ (A => B)) => B

p5 :: Prop
p5 = Disj (Imply (Var 'A') (Var 'B')) (Imply (Var 'B') (Var 'A'))

--Equiv
p6 :: Prop
p6 = Equiv (Imply (Var 'A') (Var 'B')) (Disj (Not (Var 'A')) (Var 'B'))

spec :: Spec
spec = do     
  describe "isTaut" $ do
    it "p1 is False" $
      isTaut p1 `shouldBe` False

    it "p2 is True" $
      isTaut p2 `shouldBe` True

    it "p3 is False" $
      isTaut p3 `shouldBe` False

    it "p4 is True" $
      isTaut p4 `shouldBe` True
    
    it "p5 is True" $
      isTaut p5 `shouldBe` True
    
    it "p6 is True" $
      isTaut p6 `shouldBe` True