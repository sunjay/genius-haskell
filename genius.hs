-- A Genius Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module Genius (
    Piece,
    GeniusTicTacToe,
    TicTacToe,
    new,
    newBoard
) where

data Piece = PieceX | PieceO deriving (Show, Eq)

data TicTacToe = TicTacToe {
    boardTiles :: [[Maybe Piece]],
    boardWinner :: Maybe Piece
} deriving (Show)

data GeniusTicTacToe = GeniusTicTacToe {
    boards :: [[TicTacToe]],
    winner :: Maybe Piece
} deriving (Show)

-- Create a new game
new :: GeniusTicTacToe
new = GeniusTicTacToe {boards=replicate size $ replicate size boardNew, winner=Nothing}
    where size = 3

-- Creates a new board
boardNew :: TicTacToe
boardNew = TicTacToe {boardTiles=replicate size $ replicate size Nothing, boardWinner=Nothing}
    where size = 3

-- gets the winner in this row or 0 if there is no winner
-- using 1 as x and -1 as o and 0 as empty tile
--winner :: (Eq a, Num a) => [a] -> a
--winner row = foldl1 (\acc x -> if acc == x then x else 0) row

