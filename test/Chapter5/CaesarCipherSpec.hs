module Chapter5.CaesarCipherSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter5.CaesarCipher

--test on single module: stack test --test-arguments "-m "Chapter5.CaesarCipher"" 

spec :: Spec
spec = do
  describe "let2int" $ do
    it "returns 0 when given 'a'" $
      let2int 'a' `shouldBe` 0

    it "returns 25 when given 'z'" $
      let2int 'z' `shouldBe` 25
  
  describe "int2let" $ do
    it "returns 'a' when given 0" $
      int2let 0 `shouldBe` 'a'
    it "returns 'z' when given 25" $
      int2let 25 `shouldBe` 'z'

  describe "shift" $ do
    it "returns 'd' when shifting 3 positions on 'a'" $
      shift 3 'a' `shouldBe` 'd'

    it "returns 'c' when shifting 3 positions on 'z'" $
      shift 3 'z' `shouldBe` 'c'

    it "returns 'z' when shifting -3 positions on 'c'" $
      shift (-3) 'c' `shouldBe` 'z'

    it "returns ' ' when shifting 3 positions on ' '" $
      shift 3 ' ' `shouldBe` ' '

  describe "encode" $ do
    it "returns 'kdvnhoo lv ixq' when shifting 3 positions and given 'haskell is fun'" $
      encode 3 "haskell is fun" `shouldBe` "kdvnhoo lv ixq"
    
    it "returns 'haskell is fun' when shifting -3 positions and given 'kdvnhoo lv ixq'" $
      encode (-3) "kdvnhoo lv ixq" `shouldBe` "haskell is fun"

    it "encode 'Think like a Fundamentalist Code like a Hacker'" $ 
      encode 13 "Think like a Fundamentalist Code like a Hacker" `shouldBe` "Guvax yvxr n Shaqnzragnyvfg Pbqr yvxr n Unpxre"

  describe "percent" $ do
    it "returns 33.333336 when given 5 15" $
      percent 5 15 `shouldBe` 33.333336

  describe "freqs" $ do
    it "returns frequency list when given a string" $
      let fs = freqs "abbcccddddeeeee"
      in do
            fs !! 0 `shouldBe` 6.666667
            fs !! 1 `shouldBe` 13.333334
            fs !! 2 `shouldBe` 20.0
            fs !! 3 `shouldBe` 26.666668
            fs !! 25 `shouldBe` 0.0
         

  describe "rotate" $ do
    it "returns [4,5,1,2,3] when rotating 3 on [1,2,3,4,5]" $
      rotate 3 [1,2,3,4,5] `shouldMatchList` [4,5,1,2,3]

  describe "crack" $ do
    it "returns 'haskell is fun' when given 'kdvnhoo lv ixq'" $
      crack "kdvnhoo lv ixq" `shouldBe` "haskell is fun"

    it "returns 'list comprehensions are useful' when given 'vscd mywzboroxcsyxc kbo ecopev'" $
      crack "vscd mywzboroxcsyxc kbo ecopev" `shouldBe` "list comprehensions are useful"
  
  describe "Exercise 10: with uppercase letters" $ do
    it "returns 'Haskell Is Fun' when given 'Kdvnhoo Lv Ixq'" $
      crack "Kdvnhoo Lv Ixq" `shouldBe` "Haskell Is Fun"

    it "returns 'List Comprehensions Are Useful' when given 'Vscd Mywzboroxcsyxc Kbo Ecopev'" $
      crack "Vscd Mywzboroxcsyxc Kbo Ecopev" `shouldBe` "List Comprehensions Are Useful"

    