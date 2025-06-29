-- Problem 34

totient :: Integer -> Int
totient n = length $ filter (coprime n) [1..(n - 1)]