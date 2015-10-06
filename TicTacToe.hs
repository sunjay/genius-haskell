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
    tile,
    move,
    diagonalTLBR,
    diagonalTRBL,
    rows,
    cols,
    diagonals,
    board_size,
    isFull
) where

import Data.List (find)
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

-- Extracts all the rows from the board
rows :: TicTacToe -> [[Maybe Piece]]
rows board = map (\n -> row n board) [0..board_size-1]

-- Extracts a column from the board
col :: Int -> TicTacToe -> [Maybe Piece]
col n board
    | n < board_size = each board_size $ drop n $ tiles board

-- Extracts all the rows from the board
cols :: TicTacToe -> [[Maybe Piece]]
cols board = map (\n -> col n board) [0..board_size-1]

-- Extracts a diagonal from the top left to the bottom right
diagonalTLBR :: TicTacToe -> [Maybe Piece]
diagonalTLBR board = map (\n -> tile n n board) [0..board_size-1]

-- Extracts a diagonal from the top left to the bottom right
diagonalTRBL :: TicTacToe -> [Maybe Piece]
diagonalTRBL board = map (\n -> tile n (board_size-n-1) board) [0..board_size-1]

-- Extracts all the rows from the board
diagonals :: TicTacToe -> [[Maybe Piece]]
diagonals board = [diagonalTLBR board, diagonalTRBL board]

-- Extracts a single tile from the board
tile :: Int -> Int -> TicTacToe -> Maybe Piece
tile rowIndex colIndex board
    | rowIndex < board_size && colIndex < board_size = tiles board !! (rowIndex * board_size + colIndex)

isFull :: TicTacToe -> Bool
-- If you don't find any Nothings, there is no empty spots
isFull board = isNothing $ find isNothing (tiles board)

-- Makes a move on the board
move :: Int -> Int -> Piece -> TicTacToe -> TicTacToe
move rowIndex colIndex piece board
    | isNothing (tile rowIndex colIndex board) && isNothing oldWinner
        = TicTacToe {
            tiles=newTiles,
            winner=if isNothing oldWinner then
                checkWinner TicTacToe {tiles=newTiles, winner=Nothing}
            else
                oldWinner
        }
        where
            oldWinner = winner board
            newTiles = map filterTiles $ zip [0..] $ tiles board
            filterTiles = (\(index, value) ->
                if (rowIndex * board_size + colIndex) == index then 
                    Just piece
                else
                    value)

-- Returns the winner of a game or Nothing if there is no winner
checkWinner :: TicTacToe -> Maybe Piece
checkWinner board = 
    let pieceSets = concat [
            rows board,
            cols board,
            diagonals board]
    in foldl (\acc pieces ->
        if isNothing acc then
            foldl1 (\acc x -> if acc == x then x else Nothing) pieces
        else
            acc) Nothing pieceSets

-- Takes each nth element from a list
each n = map head . takeWhile (not . null) . iterate (drop n)

