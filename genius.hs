-- A Genius Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

-- gets the winner in this row or 0 if there is no winner
-- using 1 as x and -1 as o and 0 as empty tile
winner :: (Eq a, Num a) => [a] -> a
winner row = foldl1 (\acc x -> if acc == x then x else 0) row

