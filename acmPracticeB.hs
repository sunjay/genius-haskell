-- Practice: Problem B

-- scores contains triples of (the problem name, time submitted, correct/incorrect)
-- scores :: (Num a) => [(String, a, String)]

-- Returns the solution to the problem
problemB :: (Num a) => [(String, a, String)] -> (a, a)
problemB scores = (solved scores, penalty scores)

-- Returns the number of problems solved
solved :: (Num a) => [(String, a, String)] -> a
solved scores = sum [1 | (_, _, status) <- scores, status == "correct"]

-- Returns the penalty awarded for incorrect responses on eventually correct questions
penalty :: (Num a) => [(String, a, String)] -> a
penalty scores = sum [(incorrect_count prob scores) * 20 + time | (prob, time, status) <- scores, status == "correct"]

-- Returns the number of incorrect values for a given problem
incorrect_count :: (Num a) => String -> [(String, a, String)] -> a
incorrect_count _ [] = 0
incorrect_count problem ((p, _, status):xs)
    | p == problem && status == "incorrect" = 1 + rest_count
    | otherwise = rest_count
    where rest_count = incorrect_count problem xs

