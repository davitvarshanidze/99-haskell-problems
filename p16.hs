-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter ((n/=) . snd) $ zip xs [1..]