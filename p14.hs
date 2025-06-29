-- Problem 14

dupli :: [a] -> [a]
dupli = concatMap (replicate 2) 
