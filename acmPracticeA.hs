-- Practice: Problem A

oddsUpTo :: (Num a) => a -> [a]
oddsUpTo n = [x | x <- [1, 3..n-1]]

