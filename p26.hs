-- Problem 26

combinations :: Int -> [a] -> [[a]]
combinations 0 xs     = []
combinations 1 xs     = map (:[]) xs
combinations n []     = []
combinations n (x:xs) = (map (x:) $ combinations (n - 1) xs) ++ combinations n xs