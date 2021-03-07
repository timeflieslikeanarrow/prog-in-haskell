module Chapter15.Exercise where

--Exercise 4
fibs :: [Integer]
fibs = [0, 1] ++ [ x + y | (x, y) <- zip fibs (tail fibs)]

--Exercise 6
sqroots :: Double -> [Double]
sqroots n = iterate (\x ->  (x + n / x) / 2) 1.0

sqroot :: Double -> Double
sqroot n = let roots = sqroots n
         in head [ y | (x, y) <- zip roots (tail roots), abs (x - y) < 0.0001]


