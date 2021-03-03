module Chapter7.Base where

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

--Exercise 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x        = []
               | otherwise  = h x : unfold p h t (t x) 