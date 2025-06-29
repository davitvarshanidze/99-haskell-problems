-- Problem 39

primesR :: Integer -> Integer -> [Integer]
primesR a b = dropWhile (< a) . takeWhile (<= b) $ primes
