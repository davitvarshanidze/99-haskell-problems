-- Problem 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs