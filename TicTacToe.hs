-- A Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module TicTacToe (
    Piece (PieceX, PieceO),
    TicTacToe,
    empty,
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

import Data.Maybe (isNothing)
import Data.Matrix (Matrix)
import Data.Vector (Vector)

import qualified Data.Matrix as M

board_size = 3

data Piece = PieceX | PieceO deriving (Eq)

instance Show Piece where
    show PieceX = "x"
    show PieceO = "o"

type TicTacToe = Matrix (Maybe Piece)
type Pieces = Vector (Maybe Piece)

-- Creates an empty board
empty :: TicTacToe
empty = M.matrix board_size board_size $ \(i, j) -> Nothing

-- Extracts a row from the board
row :: Int -> TicTacToe -> Pieces
row = M.getRow
-- Extracts a column from the board
col :: Int -> TicTacToe -> Pieces
col = M.getCol
-- Extracts a single tile from the board
tile :: Int -> Int -> TicTacToe -> Maybe Piece
tile = M.getElem
-- Extracts a diagonal from the top left to the bottom right
diagonalTLBR :: TicTacToe -> Pieces
diagonalTLBR = M.getDiag
-- Extracts a diagonal from the top left to the bottom right
diagonalTRBL :: TicTacToe -> Pieces
diagonalTRBL = M.getDiag . M.switchCols 1 3

-- Extracts all the rows from the board
rows :: TicTacToe -> [Pieces]
rows board = map (\i -> row i board) [1..board_size]
-- Extracts all the columns from the board
cols :: TicTacToe -> [Pieces]
cols board = map (\i -> col i board) [1..board_size]
-- Extracts all the diagonals from the board
diagonals :: TicTacToe -> [Pieces]
diagonals board = [diagonalTLBR board, diagonalTRBL board]

-- All the rows, columns and diagonals from the board
pieceSets :: TicTacToe -> [Pieces]
pieceSets board = concat [rows board, cols board, diagonals board]

-- Returns whether the board is full
isFull :: TicTacToe -> Bool
isFull board = foldl (\acc x -> if isNothing x then False else acc) True $ M.toList board

-- Makes a move on the board
move :: Int -> Int -> Piece -> TicTacToe -> TicTacToe
move rowIndex colIndex piece board
    | isNothing (tile rowIndex colIndex board)
        = M.setElem (Just piece) (rowIndex, colIndex) board
    | otherwise = error "Tile is not empty"

-- Retrieves the winner of the board or Nothing
winner :: TicTacToe -> Maybe Piece
winner board = foldl1 (\acc w -> if isNothing acc then w else acc) winners
    where winners = map checkRowWinner $ pieceSets board

-- Checks a single row of pieces for a winner
checkRowWinner :: Pieces -> Maybe Piece
checkRowWinner = foldl1 (\acc x -> if acc == x then x else Nothing)

