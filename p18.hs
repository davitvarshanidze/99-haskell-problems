-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice xs n n' = take (n'-n+1) $ drop (n-1) xs 