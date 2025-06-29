-- Problem 36

primeFactorsMulti :: Integer -> [(Integer, Int)]
primeFactorsMulti = map swap . encode . primeFactors
                    where
                    swap (x, y) = (y, x)