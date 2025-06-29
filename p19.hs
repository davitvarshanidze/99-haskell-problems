-- Problem 19

rotate :: [a] -> Int -> [a]
rotate xs n | n >= 0 = drop n xs ++ take n xs
            | n < 0 = drop len xs ++ take len xs
                      where len = n+length xs
                            
rotate' xs n = drop n' xs ++ take n' xs 
  where n' = n `mod` length xs