module Chapter7.Code where

import Prelude hiding (map, filter)

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
--twice f x = f (f x)
twice f  = f . f

map :: (a -> b) -> [a] -> [b]
--map f xs = [f x | x <- xs]
map f []      = []
map f (x:xs)  = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
--filter p xs = [x | x <- xs, p x]
filter p [] = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs

sumsqreven :: [Int] -> Int
--sumsqreven ns = sum (map (^2) (filter even ns))
sumsqreven = sum . map (^2) . filter even