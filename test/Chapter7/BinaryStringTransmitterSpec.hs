module Chapter7.BinaryStringTransmitterSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter7.BinaryStringTransmitter

spec :: Spec
spec = do
  describe "bin2int" $ do
    it "[1,0,1,1] (reversed) should be 13" $ do
      bin2int [1,0,1,1] `shouldBe` 13

  describe "int2bin" $ do
    it "returns [1,0,1,1] when given 13" $ do
      int2bin 13 `shouldMatchList` [1,0,1,1]

  describe "make8" $ do
    it "returns 8 bits for each binary numbers" $ do
      make8 [1,0,1,1] `shouldMatchList` [1,0,1,1,0,0,0,0]   

  describe "encode" $ do
    it "encodes 'abc'" $ do
      encode "abc" `shouldMatchList` [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

  describe "chop8" $ do
    it "chop bits into lists of 8 bits" $ do
      chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] `shouldMatchList` [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]

  describe "decode" $ do
    it "decodes to 'abc'" $ do
       decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] `shouldBe` "abc"

  describe "transmit" $ do
    it "returns the original string" $ do
      transmit "higher-order functions are easy" `shouldBe` "higher-order functions are easy" 

  describe "Exercise 8: transmit'" $ do
    it "should throw an error" $
      transmit' "higher-order functions are easy" `shouldBe` "higher-order functions are easy" 