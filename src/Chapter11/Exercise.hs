module Chapter11.Exercise where

import Chapter11.TicTacToe

--Exercise 1
countGameTreeNode :: Tree Grid -> Int
countGameTreeNode (Node _ ts) = 1 + sum (map countGameTreeNode ts)

maxDepth :: Tree Grid -> Int
maxDepth (Node _ []) = 0
maxDepth (Node _ ts) = 1 + maximum (map maxDepth ts)