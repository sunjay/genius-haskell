-- A Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module TicTacToe (
    Piece,
    PieceX,
    PieceO,
    TicTacToe,
    empty,
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

import Data.Maybe (isNothing)

import Pieces (board_size, Piece (PieceX, PieceO), Pieces)
import qualified Pieces as P

data TicTacToe = TicTacToe {
    tiles :: Pieces,
    winner :: Maybe Piece
} deriving (Show)

-- Creates an empty board
empty :: TicTacToe
empty = fromPieces $ replicate (board_size*board_size) Nothing

-- Creates a board from the given list of pieces
fromPieceList :: [Maybe Piece] -> TicTacToe
fromPieceList = fromPieces . P.fromPieceList

-- Creates a board from the given sequence of pieces
fromPieces :: Pieces -> TicTacToe
fromPieces tiles' = TicTacToe {tiles=tiles', winner=[TODO: Check winner]}
        let oldWinner = winner board
            newWinner = 
                if isNothing oldWinner then
                    checkWinner $ pieceSets board
                else
                    oldWinner

-- Extracts a row from the board
row :: Int -> TicTacToe -> Pieces
row n = tilesRow $ tiles
-- Extracts a column from the board
col :: Int -> TicTacToe -> Pieces
col n = tilesCol $ tiles
-- Extracts a diagonal from the top left to the bottom right
diagonalTLBR :: TicTacToe -> Pieces
diagonalTLBR = tilesDiagonalTLBR $ tiles
-- Extracts a diagonal from the top left to the bottom right
diagonalTRBL :: TicTacToe -> Pieces
diagonalTRBL = tilesDiagonalTRBL $ tiles

-- Extracts all the rows from the board
rows :: TicTacToe -> [Pieces]
rows board = map (\n -> row n board) [0..board_size-1]
-- Extracts all the rows from the board
cols :: TicTacToe -> [Pieces]
cols board = map (\n -> col n board) [0..board_size-1]
-- Extracts all the rows from the board
diagonals :: TicTacToe -> [Pieces]
diagonals board = [diagonalTLBR board, diagonalTRBL board]

-- All the rows, columns and diagonals from the board
pieceSets :: TicTacToe -> [Pieces]
pieceSets board = concat [rows board, cols board, diagonals board]


-- Makes a move on the board
move :: Int -> Int -> Piece -> TicTacToe -> TicTacToe
move rowIndex colIndex piece board
    | isNothing (tile rowIndex colIndex board)
        = fromPieces $ S.update (rowIndex * board_size + colIndex) (Just piece) (tiles board)

checkWinner :: [[Maybe Piece]] -> Maybe Piece
checkWinner pieceSets' = foldr (\pieces acc ->
        if isNothing acc then
            checkRowWinner pieces
        else
            acc) Nothing pieceSets'

checkRowWinner :: [Maybe Piece] -> Maybe Piece
checkRowWinner = foldl1 (\acc x -> if acc == x then x else Nothing)

