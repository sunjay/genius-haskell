-- A Genius Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module Genius (
    GeniusTicTacToe,
    TicTacToe,
    Piece (PieceX, PieceO),
    CurrentBoard (Any, Board),
    empty,
    currentBoard,
    boards,
    winner,
    row,
    col,
    board,
    move,
    board_size,
    isFull
) where

import qualified TicTacToe as T
import TicTacToe (TicTacToe, Piece (PieceX, PieceO), board_size)

data CurrentBoard = Any | Board Int deriving (Show, Eq)
type Boards = Seq TicTacToe

data GeniusTicTacToe = GeniusTicTacToe {
    currentBoard :: CurrentBoard,
    boards :: Boards,
    winner :: Maybe Piece
} deriving (Show)

-- Create a new game
empty :: GeniusTicTacToe
empty = GeniusTicTacToe {boards=replicate (board_size*board_size) T.empty, winner=Nothing, currentBoard=Any}

-- Extracts a row of boards from the game
row :: Int -> GeniusTicTacToe -> Boards

-- Extracts a column of boards from the game
col :: Int -> GeniusTicTacToe -> Boards

-- Extracts a single board from the game
board :: Int -> Int -> GeniusTicTacToe -> TicTacToe
board rowIndex colIndex game
    | rowIndex < board_size && colIndex < board_size = boards game !! (rowIndex * board_size + colIndex)

-- Makes a move on the current board or on any of the boards if the current board is any
-- If the current board is any then the indexes should be between 0 and board_size*board_size, otherwise they should be less than board_size and correspond to a position on the current board
move :: Int -> Int -> Piece -> GeniusTicTacToe -> GeniusTicTacToe
move rowIndex colIndex piece game
    | isNothing oldWinner =
        if current == Any then
            let localBoard = board (row `quot` board_size) (col `quot` board_size) game
                localRow = row `mod` board_size
                localCol = col `mod` board_size
                newBoard = T.move localRow localCol piece localBoard
                --TODO
    where
        oldWinner = winner game
        current = currentBoard game


