-- Problem 5

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x] -- naive

myReverse' =  foldl (flip (:)) [] -- Prelude version
