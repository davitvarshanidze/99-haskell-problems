-- Problen 12
    
decodeModified :: [EncodingList a] -> [a]
decodeModified = concatMap decodeHelper 
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x
