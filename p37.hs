-- Problem 37

totient' :: Integer -> Integer
totient' = foldr bam 1 . primeFactorsMulti
           where
           bam (p, m) acc = acc * (p - 1) * p ^ (m - 1)