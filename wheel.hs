-- Reinventing the wheel

-- maximum
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot find maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- replicate
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:(replicate' (n-1) x)

-- take
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- reverse
reverse' :: [a] -> a

-- repeat
repeat' :: a -> [a]

-- zip
zip' :: [a] -> [b] -> [(a, b)]


