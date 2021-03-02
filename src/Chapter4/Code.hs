module Chapter4.Code where

import Prelude hiding (even, splitAt, recip, abs, signum, not, (&&), fst, snd, head, tail, const)

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n

abs :: Int -> Int
abs n = if n >= 0 then n else -n

abs' :: Int -> Int
abs' n | n >= 0     = n
       | otherwise  = -n

signum :: Int -> Int
signum n = if n < 0 then -1 else 
             if n == 0 then 0 else 1

signum' :: Int -> Int
signum' n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

not :: Bool -> Bool
not False = True
not True  = False

(&&) :: Bool -> Bool -> Bool
True && b   = b
False && _  = False

--b && c | b == c = b
--       | otherwise = False

--True && True    = True
--_    && _       = False

--True && False   = False
--False && True   = False
--False && False  = False

fst :: (a, b) -> a
fst (x, _) = x 

snd :: (a, b) -> b
snd (_, y) = y

test :: [Char] -> Bool
--test ['a', _, _] = True
test ('a':_)     = True
test _           = False


head :: [a] -> a
head (x:_) = x 

tail :: [a] -> [a]
tail (_:xs) = xs

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> Int -> Int
add' = \x -> (\y -> x + y)

const :: a -> b -> a
const x _ = x

const' :: a -> (b -> a)
const' x = \_ -> x

odds :: Int -> [Int]
odds n = map f [0 .. n-1]
         where f x = x * 2 + 1

odds' :: Int -> [Int]
odds' n = map (\x -> x*2 + 1) [0..n-1]     