-- Problem 40

maybeGoldbach :: Integer -> Maybe (Integer, Integer)
maybeGoldbach n =
  let primes' = takeWhile (< n) primes
  in
    findPair primes'
  where
  findPair [] = Nothing
  findPair (p:ps) = 
    case find ((n - p) ==) (p:ps) >>= \v -> Just (p, v) of
      Nothing -> findPair ps
      result -> result

goldbach :: Integer -> (Integer, Integer)
goldbach n =
  case maybeGoldbach n of
    Nothing -> error "Must be true for all small even n"
    Just (a,b) -> (a,b)