module Pieces (
    Piece (PieceX, PieceO),
    Pieces,
    fromPieceList,
    row,
    col,
    tile,
    diagonalTLBR,
    diagonalTRBL,
    rows,
    cols,
    diagonals,
    pieceSets,
    board_size,
    isFull
) where

import Data.Maybe (isNothing)
import qualified Data.Sequence as S
import Data.Sequence (Seq)

board_size = 3

data Piece = PieceX | PieceO deriving (Show, Eq)

type Pieces = Seq (Maybe Piece)

-- Creates a board from the given list of pieces
fromPieceList :: [Maybe Piece] -> Pieces
fromPieceList = S.fromList

-- Extracts a row from the board
row :: Int -> Pieces -> Pieces
row n tiles'
    | n < board_size = S.take board_size $ S.drop (n*board_size) tiles'
-- Extracts a column from the board
col :: Int -> Pieces -> Pieces
col n tiles'
    | n < board_size = S.take board_size $ each board_size $ S.drop n $ tiles'
-- Extracts a diagonal from the top left to the bottom right
diagonalTLBR :: Pieces -> Pieces
diagonalTLBR tiles' = S.fromList $ map (\n -> tile n n tiles') [0..board_size-1]
-- Extracts a diagonal from the top left to the bottom right
diagonalTRBL :: Pieces -> Pieces
diagonalTRBL tiles' = S.fromList $ map (\n -> tile n (board_size-n-1) tiles') [0..board_size-1]
-- Extracts a single tile from the board
tile :: Int -> Int -> Pieces -> Maybe Piece
tile rowIndex colIndex tiles'
    | rowIndex < board_size && colIndex < board_size = S.index tiles' (rowIndex * board_size + colIndex)

isFull :: Pieces -> Bool
-- If you don't find any Nothings, there is no empty spots
isFull tiles' = isNothing $ S.findIndexL isNothing tiles'

-- Extracts all the rows from the board
rows :: Pieces -> [Pieces]
rows tiles' = map (\n -> row n tiles') [0..board_size-1]
-- Extracts all the rows from the board
cols :: Pieces -> [Pieces]
cols tiles' = map (\n -> col n tiles') [0..board_size-1]
-- Extracts all the rows from the board
diagonals :: Pieces -> [Pieces]
diagonals tiles' = [diagonalTLBR tiles', diagonalTRBL tiles']

-- All the rows, columns and diagonals from the board
pieceSets :: Pieces -> [Pieces]
pieceSets tiles' = concat [rows tiles', cols tiles', diagonals tiles']

-- Takes the nth item from items starting from the first item
each :: Int -> Seq a -> Seq a
each n items = S.fromList $ map (\i -> S.index items (n*i)) [0..]

