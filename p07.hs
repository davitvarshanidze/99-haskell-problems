-- Problem 7

-- Haskell doesn't support arbitrarily nested lists,
-- so flatten would basically be the same as 'concat'

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x