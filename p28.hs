-- Problem 28

group :: [Int] -> [a] -> [[[a]]]
group []             _      = []
group groupSizes     []     = []
group (n:[])         people = map ((:[]) . fst) (chooseAndSplit n people)
group (n:groupSizes) people =
  let firstGroupChoices = chooseAndSplit n people
  in
    foldr
      (\(firstGroupChoice, others) res ->
         res ++ map (firstGroupChoice:) (group groupSizes others))
      []
      firstGroupChoices