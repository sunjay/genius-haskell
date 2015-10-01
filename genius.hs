-- A Genius Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module Genius (
    Piece,
    GeniusTicTacToe,
    TicTacToe,
    new,
    newBoard,
    gameBoards,
    gameWinner,
    boardTiles,
    boardWinner
) where

data Piece = PieceX | PieceO deriving (Show, Eq)

data TicTacToe = TicTacToe {
    boardTiles :: [[Maybe Piece]],
    boardWinner :: Maybe Piece
} deriving (Show)

data GeniusTicTacToe = GeniusTicTacToe {
    gameBoards :: [[TicTacToe]],
    gameWinner :: Maybe Piece
} deriving (Show)

-- Create a new game
new :: GeniusTicTacToe
new = GeniusTicTacToe {gameBoards=replicate size $ replicate size newBoard, gameWinner=Nothing}
    where size = 3

-- Creates a new board
newBoard :: TicTacToe
newBoard = TicTacToe {boardTiles=replicate size $ replicate size Nothing, boardWinner=Nothing}
    where size = 3

boardRow :: TicTacToe -> Int -> [Maybe Piece]
boardRow board row = boardTiles board !! row

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

