-- A Genius Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module Genius (
    TicTacToe,
    new,
    boards,
    winner,
    board,
    move,
    board_size
) where

import qualified TicTacToe as T
import TicTacToe (Piece, PieceX, PieceO, board_size)

data GeniusTicTacToe = GeniusTicTacToe {
    boards :: [T.TicTacToe],
    winner :: [Maybe Piece]
} deriving (Show)

-- Create a new game
new :: GeniusTicTacToe
new = GeniusTicTacToe {boards=replicate (board_size*board_size) T.new, winner=Nothing}

row :: Int -> TicTacToe -> [Maybe Piece]
row n game
    | n < board_size = take board_size $ drop (n*board_size) (boards game)

-- Gets a single tile value from a board
boardTile :: TicTacToe -> Int -> Int -> Maybe Piece
boardTile board row col = boardRow board row !! col

-- Makes a move on a board and then returns a new board
boardMove :: TicTacToe -> Int -> Int -> Piece -> TicTacToe
boardMove board row col piece
    | boardTile board row col == Nothing = replaceNth row (replaceNth col piece (boardRow board row)) (boardTiles board)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newValue (x:xs)
    | n == 0 = newValue:xs
    | otherwise = x:replaceNth (n-1) newValue xs

-- gets the winner in this row or 0 if there is no winner
-- using 1 as x and -1 as o and 0 as empty tile
--winner :: (Eq a, Num a) => [a] -> a
--winner row = foldl1 (\acc x -> if acc == x then x else 0) row

