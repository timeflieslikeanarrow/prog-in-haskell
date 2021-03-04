module Chapter9.ExerciseSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter9.Code 

{-

before optimization
real    0m13.696s
user    0m42.419s
sys     0m18.839s

after optimization 1:
real    0m0.935s
user    0m1.921s
sys     0m1.007s

after optimization 2:
real    0m0.343s
user    0m0.458s
sys     0m0.146s

-}

spec :: Spec
spec = do     
  describe "Exercise 2" $ do
    it "removeFirstOccurence" $
      removeFirstOccurence 1 [2,1,3,1,4,5] `shouldMatchList` [2,3,1,4,5]

  describe "Exercise 4" $ do
    it "solution count" $
      length (solutions [1,3,7,10,25,50] 765) `shouldBe` 49 --before: 780

    it "[1,3,7,10,25,50]" $ do
      length ([ e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns']) `shouldBe` 33665406
      length ([ e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns', (not . null) (eval e)]) `shouldBe` 245644 -- before: 4672540