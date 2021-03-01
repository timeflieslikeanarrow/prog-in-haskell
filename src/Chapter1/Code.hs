module Chapter1.Code where

import Prelude hiding (sum, product)

double :: Num a => a -> a
double x = x + x

sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + sum ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]


seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

--Exercise 3
product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n * product ns

--Exercise 4
qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
               where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]
