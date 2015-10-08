-- A Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module TicTacToe (
    Piece (PieceX, PieceO),
    TicTacToe,
    fromPieces,
    fromPieceList,
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

import qualified Pieces as P
import qualified Data.Sequence as S

import Data.Maybe (isNothing)
import Data.Foldable (toList)

import Pieces (board_size, Piece (PieceX, PieceO), Pieces)

data TicTacToe = TicTacToe {
    tiles :: Pieces,
    winner :: Maybe Piece
} deriving (Show)

-- Creates an empty board
empty :: TicTacToe
empty = fromPieces $ S.replicate (board_size*board_size) Nothing

-- Creates a board from the given list of pieces
fromPieceList :: [Maybe Piece] -> TicTacToe
fromPieceList = fromPieces . P.fromPieceList

-- Creates a board from the given sequence of pieces
fromPieces :: Pieces -> TicTacToe
fromPieces tiles' = TicTacToe {tiles=tiles', winner=checkWinner $ P.pieceSets tiles'}

-- Extracts a row from the board
row :: Int -> TicTacToe -> Pieces
row n board = P.row n $ tiles board
-- Extracts a column from the board
col :: Int -> TicTacToe -> Pieces
col n board = P.col n $ tiles board
-- Extracts a single tile from the board
tile :: Int -> Int -> TicTacToe -> Maybe Piece
tile rowIndex colIndex board = P.tile rowIndex colIndex $ tiles board
-- Extracts a diagonal from the top left to the bottom right
diagonalTLBR :: TicTacToe -> Pieces
diagonalTLBR = P.diagonalTLBR . tiles
-- Extracts a diagonal from the top left to the bottom right
diagonalTRBL :: TicTacToe -> Pieces
diagonalTRBL = P.diagonalTRBL . tiles

-- Extracts all the rows from the board
rows :: TicTacToe -> [Pieces]
rows = P.rows . tiles
-- Extracts all the rows from the board
cols :: TicTacToe -> [Pieces]
cols = P.cols . tiles
-- Extracts all the rows from the board
diagonals :: TicTacToe -> [Pieces]
diagonals = P.diagonals . tiles

-- All the rows, columns and diagonals from the board
pieceSets :: TicTacToe -> [Pieces]
pieceSets = P.pieceSets . tiles

-- Returns whether the board is full
isFull :: TicTacToe -> Bool
isFull = P.isFull . tiles

-- Makes a move on the board
move :: Int -> Int -> Piece -> TicTacToe -> TicTacToe
move rowIndex colIndex piece board
    | isNothing (tile rowIndex colIndex board)
        = TicTacToe {
            tiles=newTiles,
            winner=
                if isNothing oldWinner then
                    checkWinner (P.pieceSets newTiles)
                else
                    oldWinner
        }
        where 
            oldWinner = winner board
            newTiles = S.update (rowIndex * board_size + colIndex) (Just piece) (tiles board)

-- Checks to see if there is any winner in the given piece sets
checkWinner :: [Pieces] -> Maybe Piece
checkWinner pieceSets' = foldr (\pieces acc ->
    if isNothing acc then
        checkRowWinner pieces
    else
        acc) Nothing pieceSets'

checkRowWinner :: Pieces -> Maybe Piece
checkRowWinner pieces = foldl1 (\acc x -> if acc == x then x else Nothing) $ toList pieces

