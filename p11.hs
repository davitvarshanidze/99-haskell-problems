-- Problem 11

data EncodingList a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [EncodingList a]
encodeModified = map encodeHelper . encode 
  where
    encodeHelper (1,x) = Single x 
    encodeHelper (n,x) = Multiple n x