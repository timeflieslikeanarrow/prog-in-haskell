module Chapter11.TicTacToeSpec where

import Data.List

import Test.Hspec
import Test.QuickCheck

import Chapter11.TicTacToe

row :: [Player]
row = [O, B, X]

spec :: Spec
spec = do
  describe "transpose" $ do
    it "transpose a grid" $ do
      transpose [[1,2,3],[4,5,6],[7,8,9]] `shouldMatchList` [[1,4,7], [2,5,8], [3,6,9]]

  describe "show player" $ do
    it "map showPlayer" $
      map showPlayer row `shouldMatchList` [["   ", " O ", "   "], 
                                            ["   ", "   ", "   "], 
                                            ["   ", " X ", "   "]]

    it "interleave with bar" $ 
      interleave (replicate 3 "|") (map showPlayer row) `shouldMatchList` [["   ", " O ", "   "], ["|", "|", "|"],
                                                                           ["   ", "   ", "   "], ["|", "|", "|"],
                                                                           ["   ", " X ", "   "]]
    it "beside" $
      foldr1 (zipWith (++)) (interleave (replicate 3 "|") (map showPlayer row)) `shouldMatchList` ["   |   |   ", 
                                                                                                   " O |   | X ", 
                                                                                                   "   |   |   "]