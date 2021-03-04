module Chapter10.Exercise where

import System.IO

--Exercise 1
putStr2 xs = sequence_ [putChar x | x <- xs]

--Exercise 4
adder :: IO ()
adder = do putStr "How many numbers: "
           line <- getLine
           getTotal 0 (read line)


getTotal :: Int -> Int -> IO ()
getTotal total remain = do if remain == 0 then
                              do putStrLn ("The total is " ++ (show total))
                           else
                              do line <- getLine
                                 getTotal (total + read line) (remain - 1)
                           

--Exercise 5
adder' :: IO ()
adder' = do putStr "How many numbers: "
            line <- getLine
            xs <- sequence [getLine | _ <- [1 .. read line ::Int]]
            let ns = map read xs
            putStrLn ("The total is " ++ (show (sum ns)))

--Exercise 6
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else if x == '\DEL' then
                 do putChar '\b'
                    xs <- readLine
                    return xs
              else 
                 do putChar x
                    xs <- readLine
                    return (x:xs)

