-- Problem 25

import System.Random
import Data.List

f1 :: RandomGen g => [a] -> g -> [a]
f1 g xs = let (i, _) = randomR (0, total - 1) g in permutations xs !! i
	      where fact n = product [1..n]
	            total = fact $ length xs