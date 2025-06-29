-- Problem 4

myLength :: [a] -> Int
myLength xs = sum [ 1 | _ <- xs ]

myLength' xs = snd $ last $ zip xs [1..]