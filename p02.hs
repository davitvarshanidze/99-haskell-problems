-- Problem 2

myButLast :: [a] -> a
myButLast = last . init

myButLast' = head . drop 1 . reverse -- like myLast'

myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs