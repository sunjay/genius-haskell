-- A Genius Tic-Tac-Toe implementation in Haskell --
-- Author: Sunjay Varma --

module Genius (
    TicTacToe,
    Piece (PieceX, PieceO),
    CurrentBoard (Any, Board),
    new,
    boards,
    winner,
    board,
--    move,
    board_size
) where

import qualified TicTacToe as T
import TicTacToe (TicTacToe, Piece (PieceX, PieceO), board_size)

data CurrentBoard = Any | Board Int deriving (Show, Eq)

data GeniusTicTacToe = GeniusTicTacToe {
    currentBoard :: CurrentBoard,
    boards :: [TicTacToe],
    winner :: Maybe Piece
} deriving (Show)

-- Create a new game
new :: GeniusTicTacToe
new = GeniusTicTacToe {boards=replicate (board_size*board_size) T.new, winner=Nothing, currentBoard=Any}

board :: Int -> Int -> GeniusTicTacToe -> TicTacToe
board rowIndex colIndex game
    | rowIndex < board_size && colIndex < board_size = boards game !! (rowIndex * board_size + colIndex)

--move :: Int -> Int -> Maybe Piece -> GeniusTicTacToe -> GeniusTicTacToe

