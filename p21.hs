-- Problem 21

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs (n+1) = start ++ x : end 
  where
    (start, end) = splitAt n xs