module Chapter7.VotingSpec where

import Test.Hspec
import Test.QuickCheck

import Chapter7.Base
import Chapter7.Voting

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

spec :: Spec
spec = do
  describe "first past the post system" $ do 
    describe "count" $ do
      it "count the number of times a given value occurs" $ do
        count "Red" votes `shouldBe` 2
        count "Green" votes `shouldBe` 1
        count "Blue" votes `shouldBe` 3

    describe "rmdups" $ do
      it "remove duplicates from a list" $ do
        rmdups votes `shouldMatchList` ["Red", "Blue", "Green"]

    describe "result" $ do
      it "results from votes" $ do 
        result votes `shouldMatchList` [(1, "Green"), (2, "Red"), (3, "Blue")]

    describe "winner" $ do
      it "Blue won from votes" $ do
        winner votes `shouldBe` "Blue"

  describe "alternative vote" $ do
    describe "rmempty" $ do
      it "removes the empty ballots" $ do
        rmempty [[1],[], [2]] `shouldMatchList` [[1],[2]]  

    describe "elim" $ do
      it "removes give candidate" $ do
        elim 1 [[1,2],[2,3], [1,2,3]] `shouldMatchList` [[2],[2,3],[2,3]]

    describe "rank" $ do
      it "ranks the candidates" $ do
        rank ballots `shouldMatchList` ["Red", "Blue", "Green"]

    describe "winner'" $ do
      it "returns the winner with the highest rank" $ do
        winner' ballots `shouldBe` "Green"
