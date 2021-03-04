module Chapter7.Exercise where

import Prelude hiding (all, any, takeWhile, dropWhile)

import Chapter7.Base

--Exercise 2
all :: (a -> Bool) -> [a] -> Bool
--all _ []                  = True
--all f (b:bs) | f b        = all f bs
--             | otherwise  = False

--all f = foldr (\x r -> f x && r)  True
all f = and . map f 

any :: (a -> Bool) -> [a] -> Bool
--any _ []                  = False
--any f (b:bs) | f b        = True
--             | otherwise  = any f bs

--any f = foldr (\x r -> f x || r) False
any f = or . map f

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []                  = []
takeWhile f (x:xs) | f x        = x : takeWhile f xs
                   | otherwise  = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []                  = []
dropWhile f (x:xs) | f x        = dropWhile f xs 
                   | otherwise  = x:xs


--Exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\r e -> r * 10 + e) 0

--Exercise 5
curry f = \x -> (\y -> f (x, y))
uncurry f = \(x, y) -> f x y

--Exercise 6
map' f = unfold null (f . head) tail 

iterate' f = unfold (const False) id f

--Exercise 9
altMap f g []     = []
altMap f g (x:xs) = f x : altMap g f xs 

--Exercise 10
luhnDouble :: Int -> Int
luhnDouble n = let result = 2 * n
               in 
               if result > 9 then result - 9 else result

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . altMap id luhnDouble . reverse

