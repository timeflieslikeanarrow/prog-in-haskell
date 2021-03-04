module Chapter7.Base where

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

type Bit = Int

bin2int :: [Bit] -> Int
--bin2int bits = sum [w*b | (w, b) <- zip weights bits]
--                where weights = iterate (*2) 1

bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
--int2bin 0 = []
--int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

--Exercise 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x        = []
               | otherwise  = h x : unfold p h t (t x) 

