module Chapter12.CodeSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter12.Code


spec :: Spec
spec = do
  describe "Functor" $ do
    describe "Tree" $ do
      it "Leaf" $  
        fmap length (Leaf "abc") `shouldBe` Leaf 3
      it "Node" $
        fmap even (Node (Leaf 1) (Leaf 2)) `shouldBe` Node (Leaf False) (Leaf True)

    describe "inc1" $ do
      it "Maybe" $
        inc1 (Just 1) `shouldBe` Just 2
      it "list" $
        inc1 [1,2,3,4,5] `shouldBe` [2,3,4,5,6]
      it "Tree" $
        inc1 (Node (Leaf 1) (Leaf 2)) `shouldBe` Node (Leaf 2) (Leaf 3)
  
  describe "Applicative" $ do
    describe "Maybe" $ do
      it "1 parameter" $
        pure (+1) <*> Just 1 `shouldBe` Just 2
      it "2 parameters" $
        pure (+) <*> Just 1 <*> Just 2 `shouldBe` Just 3
      it "Nothing" $
        pure (+) <*> Nothing <*> Just 2 `shouldBe` Nothing

    describe "[]" $ do
      it "1 parameter" $ 
        pure (+1) <*> [1,2,3] `shouldMatchList` [2,3,4]
      it "2 parameters" $ do
        pure (+) <*> [1] <*> [2] `shouldMatchList` [3]
        pure (*) <*> [1,2] <*> [3,4] `shouldMatchList` [3,4,6,8]

  describe "eval'" $ do
    it "div 1  0" $
      eval' (Div (Val 1) (Val 0)) `shouldBe` Nothing

  describe "eval'' with >>=" $ do
    it "div 1  0" $
      eval'' (Div (Val 1) (Val 0)) `shouldBe` Nothing

  describe "eval''' with do" $ do
    it "div 1  0" $
      eval''' (Div (Val 1) (Val 0)) `shouldBe` Nothing
  
  describe "paris" $ do
    it "pairs [1,2] [3,4]" $
      pairs [1,2] [3,4] `shouldMatchList` [(1,3), (1,4), (2, 3), (2,4)]

  describe "state Monad" $ do
    it "rlabel" $
      fst (rlabel tree 0) `shouldBe` Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

    it "alabel" $
      fst (app (alabel tree) 0) `shouldBe` Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

    it "mlabel" $
      fst (app (mlabel tree) 0) `shouldBe` Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

  describe "mapM" $ do
    it "conv" $ do
      mapM conv "1234" `shouldBe` Just [1,2,3,4]
      mapM conv "123a" `shouldBe` Nothing
  
  describe "filterM" $ do
    it "[1,2,3]" $
      filterM (\x -> [True, False]) [1,2,3] `shouldMatchList` [[1,2,3], [1,2],[1,3], [1], [2, 3], [2], [3], []]

  describe "join" $ do
    it "list" $
      join [[1,2],[3,4],[5,6]] `shouldMatchList` [1,2,3,4,5,6]
    it "Maybe" $ do 
      join (Just (Just 1)) `shouldBe` Just 1
      join (Just (Nothing::Maybe Int)) `shouldBe` Nothing
      join (Nothing::Maybe (Maybe Int)) `shouldBe` Nothing
    
      