-- Problem 35

primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n primes
                 where
                 primeFactors' n ps | n < 2 = []
                                    | otherwise = let (p:ps) = dropWhile (not . (0 ==) . mod n) primes 
                                                  in p : primeFactors' (div n p) (p:ps)