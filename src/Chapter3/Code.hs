module Chapter3.Code where

import Prelude hiding (last, init)

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> Int -> Int
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

zeroto :: Int -> [Int]
zeroto n = [0..n]

--Exercise 2
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2], [3,4,5,6]]

add2 :: Int -> Int -> Int -> Int
add2 x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a

--Exercise 3
seconds :: [a] -> a
seconds xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)