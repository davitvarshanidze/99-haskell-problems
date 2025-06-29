-- Problem 22

range :: Int -> Int -> [Int]
range n m | n < m     = take (m-n+1) $ iterate (+1) n 
          | n == m    = [n]
          | otherwise = take (n-m+1) $ iterate (+(-1)) n