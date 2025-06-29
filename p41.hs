-- Problem 41

andP f g (x,y) = f x && g y

goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList lower upper 
  | (lower `mod` 2) /= 0 = goldbachList (lower + 1) upper
  | lower > upper = []
  | otherwise = goldbach lower : goldbachList (lower + 2) upper

goldbachList' lower upper lowerPrime
  | lowerPrime * 2 > lower = goldbachList' (lowerPrime * 2) upper lowerPrime
  | otherwise =
    let biggerThanLowerPrime = (>= lowerPrime)
    in
      filter (andP biggerThanLowerPrime biggerThanLowerPrime) $ goldbachList lower upper