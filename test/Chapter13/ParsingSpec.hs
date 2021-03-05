module Chapter13.ParsingSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Control.Applicative

import Data.Char
import Chapter13.Parsing

spec :: Spec
spec = do
  describe "item" $ do
    it "empty string" $  
      parse item "" `shouldBe` []
    it "abc" $
      parse item "abc" `shouldBe` [('a', "bc")]
  
  describe "Functor" $ do
    it "empty string" $
      parse (fmap toUpper item) "" `shouldBe` []
    it "toUpper" $
      parse (fmap toUpper item) "abc" `shouldBe` [('A', "bc")]  

  describe "Applicative" $ do
    it "pure" $
      parse (pure 1) "abc" `shouldBe` [(1, "abc")]   

    it "three" $ do
      parse three "abcdef" `shouldBe` [(('a', 'c'), "def")] 
      parse three "ab" `shouldBe` [] 

    it "three'" $ do
      parse three' "abcdef" `shouldBe` [(('a', 'c'), "def")] 
      parse three' "ab" `shouldBe` [] 

  describe "Alternative" $ do
    --it "empty" $
    --  parse empty "abc" `shouldBe` []

    it "<|>" $ do
      parse (item <|> return 'd') "abc" `shouldBe` [('a', "bc")]
      parse (empty <|> return 'd') "abc" `shouldBe` [('d', "abc")]

  describe "sat" $ do
    it "char" $
      parse (char 'a') "abc" `shouldBe` [('a', "bc")]

    it "string" $ do
      parse (string "abc") "abcdef" `shouldBe` [("abc", "def")]
      parse (string "abc") "ab1234" `shouldBe` []

    it "many" $ do
      parse (many digit) "123abc" `shouldBe` [("123", "abc")]
      parse (many digit) "abc" `shouldBe` [("", "abc")]
    
    it "some" $ do
      parse (some digit) "abc" `shouldBe` []

    it "ident" $
      parse ident "abc def" `shouldBe` [("abc", " def")]

    it "nat" $ 
      parse nat "123 abc" `shouldBe` [(123, " abc")]

    it "space" $ 
      parse space "    abc" `shouldBe` [((), "abc")]

    it "int" $
      parse int "-123 abc" `shouldBe` [(-123, " abc")]

    it "nats" $ do
      parse nats " [1, 2, 3] " `shouldBe` [([1,2,3], "")]
      parse nats " [1, 2, ] " `shouldBe` []
  
  describe "eval" $ do
    it "2*3+4" $
      eval "2*3+4" `shouldBe` 10

    it "2*(3+4)" $
      eval "2*(3+4)" `shouldBe` 14

    it "2*3^4" $
      evaluate (eval "2*3^4") `shouldThrow` errorCall "Unused input ^4"  --anyException

    it "one plus two" $
      evaluate (eval "one plus two") `shouldThrow` errorCall "Invalid input"

    --Exercise 6
    it "2*3 - 4" $
      eval "2*3 - 4" `shouldBe` 2

    it "2*(3-4)" $
      eval "2*(3-4)" `shouldBe` (-2)

    it "(2*3) / 4" $
      eval "(2*3) / 4" `shouldBe` 1