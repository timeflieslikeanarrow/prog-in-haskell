module Chapter2.Code where

import Prelude hiding (last, init)

--Exercise 4
last :: [a] -> a
--last xs = head (drop (length xs - 1) xs)
--last xs = xs !! (length xs - 1)
last [x] = x
last (_:xs) = last xs

--Exercise 5
init :: [a] -> [a]
--init xs = take (length xs - 1) xs
--init xs = reverse (tail (reverse xs))
init [_] = []
init (x:xs) = x : init xs