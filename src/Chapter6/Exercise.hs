module Chapter6.Exercise where

import Prelude hiding ((^), and, concat, replicate, (!!), elem)

--Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n < 0     = 0
          | otherwise = n + sumdown (n-1)


--Exercise 3
(^) :: Int -> Int -> Int
m ^ 0       = 1
m ^ n | n < 0   = 0
      | even n  = (m * m) ^ (n `div` 2)
      | otherwise = m * m ^ (n - 1)


--Exercise 4
euclid :: Int -> Int -> Int
euclid a b | a == b     = a
           | a < b      = euclid a (b - a)
           | otherwise  = euclid (a - b) b


--Exercise 5
and :: [Bool] -> Bool
and []                  = True
and (x:xs) | not x      = False
           | otherwise  = and xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
_ `elem` []                 = False
x `elem` (y:ys) | x == y    = True
                | otherwise = x `elem` ys


--Exercise 7
merge :: Ord a => [a] -> [a] -> [a]
merge []     xs                 = xs
merge xs     []                 = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys


halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

--Exercise 8
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs') (msort ys)
           where (xs', ys) = halve xs