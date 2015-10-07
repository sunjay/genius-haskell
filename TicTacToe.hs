-- A Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module TicTacToe (
    Piece (PieceX, PieceO),
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
import qualified Data.Sequence as S
import Data.Sequence (Seq)

board_size = 3

data Piece = PieceX | PieceO deriving (Show, Eq)

type Pieces = Seq (Maybe Piece)

data TicTacToe = TicTacToe {
    tiles :: Pieces,
    winner :: Maybe Piece
} deriving (Show)

-- Creates an empty board
empty :: TicTacToe
empty = fromPieces $ replicate (board_size*board_size) Nothing

-- Creates a board from the given list of pieces
fromPieceList :: [Maybe Piece] -> TicTacToe
fromPieceList = fromPieces . S.fromList

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
tilesRow :: Int -> Pieces -> Pieces
tilesRow n tiles'
    | n < board_size = S.take board_size $ S.drop (n*board_size) tiles'
-- Extracts a column from the board
tilesCol :: Int -> Pieces -> Pieces
tilesCol n tiles'
    | n < board_size = take board_size $ each board_size $ drop n $ tiles'
-- Extracts a diagonal from the top left to the bottom right
tilesDiagonalTLBR :: Pieces -> Pieces
tilesDiagonalTLBR tiles' = map (\n -> tilesTile n n tiles') [0..board_size-1]
-- Extracts a diagonal from the top left to the bottom right
tilesDiagonalTRBL :: Pieces -> Pieces
tilesDiagonalTRBL tiles' = map (\n -> tilesTile n (board_size-n-1) tiles') [0..board_size-1]

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

-- Extracts a single tile from the board
tile :: Int -> Int -> TicTacToe -> Maybe Piece
tile rowIndex colIndex board
    | rowIndex < board_size && colIndex < board_size = index (tiles board) (rowIndex * board_size + colIndex)

isFull :: TicTacToe -> Bool
-- If you don't find any Nothings, there is no empty spots
isFull board = isNothing $ S.findIndexL isNothing (tiles board)

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

-- Takes the nth item from items starting from the first item
each :: Int -> Seq a -> [a]
each n items = map (\i -> index items (n*i)) [0..]

