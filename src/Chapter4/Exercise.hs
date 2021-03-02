module Chapter4.Exercise where

import Prelude hiding ((&&), (||))

--Exercise 1
halve :: [a] -> ([a], [a])
halve xs = let n = length xs `div` 2
           in (take n xs, drop n xs)

--Exercise 2
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:_) = x

--Exercise 3
safetail :: [a] -> [a]
safetail xs = if null xs then [] 
              else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs   = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' []       = []
safetail'' (_:xs)   = xs

--Exercise 4
(||) :: Bool -> Bool -> Bool
True || True    = True
True || False   = True
False || True   = True
False || False  = False

(|||) :: Bool -> Bool -> Bool
False ||| False  = False
_     ||| _      = True

(||||) :: Bool -> Bool -> Bool
False |||| b      = b
_     |||| _      = True

(|||||) :: Bool -> Bool -> Bool
b ||||| c | b == c    = b 
          | otherwise = True

--Exercise 5
(&&) :: Bool -> Bool -> Bool
b && c =  if b then 
              if c then True 
              else False 
          else False

--Exercise 6
(&&&) :: Bool -> Bool -> Bool
b &&& c = if b then c else False

--Exercise 8
luhnDouble :: Int -> Int
luhnDouble n = let result = 2 * n
               in 
               if result > 9 then result - 9 else result

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = sum [luhnDouble w, x, luhnDouble y, z] `mod` 10 == 0