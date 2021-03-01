import Test.QuickCheck

import Chapter1

prop_double :: Int -> Bool
prop_double val = double val == 2 * val

main :: IO ()
main = do
  {--putStrLn ""
  putStrLn $ if double 2 == 4 then "OK" else "FAIL!"
  putStrLn $ if not $ double 3 == 5 then "OK" else "FAIL!"
  putStrLn $ if not $ double 5 == 7 then "OK" else "FAIL!"
  --}
  quickCheck prop_double
  putStrLn "Done"
