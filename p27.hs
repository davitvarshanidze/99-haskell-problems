-- Problem 27

chooseAndSplit :: Int -> [a] -> [([a], [a])]
chooseAndSplit 0 xs     = [([], xs)]
chooseAndSplit k []     = [([], [])]
chooseAndSplit k (x:xs) =
  let choicesWithX    = map (mapFst (x:)) $ chooseAndSplit (k - 1) xs
  in
    if k <= length xs
    then
      let choicesWithoutX = map (mapSnd (x:)) $ chooseAndSplit k xs
      in
        choicesWithoutX ++ choicesWithX
    else choicesWithX