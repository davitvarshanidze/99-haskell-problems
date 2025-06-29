-- Problem 10

-- using Problem 9 as requested
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

-- quick and very dirty
encode' []     = []
encode' (x:xs) = ((+1) $ length $ takeWhile (==x) xs, x) : 
                (encode $ dropWhile (==x) xs)