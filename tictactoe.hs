-- A Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module TicTacToe (
    Piece (PieceX, PieceO),
    TicTacToe,
    new,
    tiles,
    winner,
    row,
    col,
    --tile,
    --move,
    --diagonalTLBR,
    --diagonalTRBL
) where

import Data.Maybe (isNothing)

board_size = 3

data Piece = PieceX | PieceO deriving (Show, Eq)

data TicTacToe = TicTacToe {
    tiles :: [Maybe Piece],
    winner :: Maybe Piece
} deriving (Show)

-- Creates a new board
new :: TicTacToe
new = TicTacToe {tiles=replicate (board_size*board_size) Nothing, winner=Nothing}

-- Extracts a row from the board
row :: Int -> TicTacToe -> [Maybe Piece]
row n board
    | n < board_size = take board_size $ drop (n*board_size) (tiles board)

-- Extracts a column from the board
col :: Int -> TicTacToe -> [Maybe Piece]
col n board
    | n < board_size = each board_size $ drop n $ tiles board

-- Extracts a diagonal from the top left to the bottom right
diagonalTLBR :: TicTacToe -> [Maybe Piece]
diagonalTLBR board = map (\n -> tile n n board) [0..board_size-1]

-- Extracts a diagonal from the top left to the bottom right
diagonalTRBL :: TicTacToe -> [Maybe Piece]
diagonalTRBL board = map (\n -> tile n (board_size-n) board) [0..board_size-1]

-- Extracts a single tile from the board
tile :: Int -> Int -> TicTacToe -> Maybe Piece
tile rowIndex colIndex board
    | rowIndex < board_size && colIndex < board_size = tiles board !! (rowIndex * board_size + colIndex)

-- Makes a move on the board
move :: Int -> Int -> Maybe Piece -> TicTacToe -> TicTacToe
move rowIndex colIndex piece board
    | rowIndex < board_size && colIndex < board_size && not (isNothing piece) && isNothing (winner board)
        = TicTacToe {
            tiles=map filterTiles $ zip [0..] $ tiles board,
            winner=winner board
        }
        where filterTiles = (\(index, value) ->
                if (rowIndex * board_size + colIndex) == index then 
                    piece
                else
                    value)

-- Takes each nth element from a list
each n = map head . takeWhile (not . null) . iterate (drop n)

