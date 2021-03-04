module Chapter8.Exercise where

--Exercise 3
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

t2 :: Tree2 Int
t2 = Node2 (Node2 (Leaf2 1) (Node2 (Leaf2 4) (Leaf2 5))) 
           (Node2 (Leaf2 6) (Leaf2 9))

t3 :: Tree2 Int
t3 = Node2 (Node2 (Leaf2 1) (Node2 (Leaf2 4) (Leaf2 5))) 
           (Leaf2 9)

balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) = balanced l && balanced r && abs (leafCount l - leafCount r) <= 1

leafCount :: Tree2 a -> Int
leafCount (Leaf2 _) = 1
leafCount (Node2 l r) = leafCount l + leafCount r 

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree2 a 
balance [x] = Leaf2 x 
balance xs  = Node2 (balance ls) (balance rs)
          where (ls, rs) = halve xs






