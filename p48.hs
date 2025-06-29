-- Problem 48

-- choose all combinations of 1 from each sub array
-- choose [[a,b], [c,d]] == [[a,c], [a,d], [b,c], [b,d]]
choose :: [[a]] -> [[a]]
choose []       = []
choose (xs:[])  = map (:[]) xs
choose (xs:xss) =
  let restOfChoices = choose xss
  in
    do subChoice <- restOfChoices
       x         <- xs
       [x:subChoice]

generateCombinations :: Int -> [a] -> [[a]]
generateCombinations n values =
  let allChoices = take n $ cycle [values]
  in
    choose allChoices

tablen n logicF =
  mapM (putStrLn . showTableRow . buildRow logicF) (generateCombinations 3 [True, False])
  where
  buildRow f = \xs -> xs ++ [f xs]
  showTableRow xs = intercalate " " $ map showBool xs