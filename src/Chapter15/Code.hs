module Chapter15.Code where

import Data.Foldable

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

sumwith :: Int -> [Int] -> Int
--sumwith v []     = v 
--sumwith v (x:xs) = sumwith (v + x) xs
--sumwith v (x:xs) = (sumwith $! (v + x)) xs
sumwith = foldl' (+)