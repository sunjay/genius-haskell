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
row :: TicTacToe -> Int -> [Maybe Piece]
row board n
    | n < board_size = take board_size $ drop (n*board_size) (tiles board)

-- Extracts a column from the board
col :: TicTacToe -> Int -> [Maybe Piece]
col board n
    | n < board_size = each board_size $ drop n $ tiles board

-- Extracts a diagonal from the top left to the bottom right
--diagonalTLBR :: TicTacToe -> [Maybe Piece]

-- Extracts a diagonal from the top left to the bottom right
--diagonalTRBL :: TicTacToe -> [Maybe Piece]

-- Extracts a single tile from the board
--tile :: TicTacToe -> Int -> Maybe Piece

-- Makes a move on the board
--move :: TicTacToe -> Int -> Int -> Piece -> TicTacToe

-- Takes each nth element from a list
each n = map head . takeWhile (not . null) . iterate (drop n)

