-- Problem 24

import System.Random
import Data.List

-- The obvious answer is P23.f1 n [1..m]
f1 :: RandomGen g => Int -> Int -> g -> [Int]
f1 n m = take n . nub . randomRs (1, m)