module Chapter15.CodeSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter15.Code

spec :: Spec
spec = do
  describe "primes" $ do
    it "first 10 primes" $  
     take 10 primes `shouldMatchList` [2,3,5,7,11,13,17,19,23,29]
    
    it "takeWhile < 10" $
      takeWhile (< 10) primes `shouldMatchList` [2,3,5,7]

  describe "sumwith" $ do
    it "[1,2,3]" $ 
      sumwith 0 [1,2,3] `shouldBe` 6