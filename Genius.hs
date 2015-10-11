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

import Data.Maybe (isNothing)

import qualified Data.Sequence as S
import Data.Sequence (Seq)

import qualified TicTacToe as T
import TicTacToe (TicTacToe, Piece (PieceX, PieceO), board_size)

import Pieces (each)

data CurrentBoard = Any | Board Int deriving (Show, Eq)
type Boards = Seq TicTacToe

data GeniusTicTacToe = GeniusTicTacToe {
    currentBoard :: CurrentBoard,
    boards :: Boards,
    winner :: Maybe Piece
} deriving (Show)

-- Create a new game
empty :: GeniusTicTacToe
empty = GeniusTicTacToe {boards=S.replicate (board_size*board_size) T.empty, winner=Nothing, currentBoard=Any}

-- Extracts a row of boards from the game
row :: Int -> GeniusTicTacToe -> Boards
row n game
    | n < board_size = S.take board_size $ S.drop (n*board_size) (boards game)
    | otherwise = S.empty

-- Extracts a column of boards from the game
col :: Int -> GeniusTicTacToe -> Boards
col n game
    | n < board_size = S.take board_size $ each board_size $ S.drop n $ boards game
    | otherwise = S.empty

-- Extracts a single board from the game
board :: Int -> Int -> GeniusTicTacToe -> TicTacToe
board rowIndex colIndex game
    | rowIndex < board_size && colIndex < board_size = S.index (boards game) (rowIndex * board_size + colIndex)

-- Returns whether the board is full
isFull :: GeniusTicTacToe -> Bool
isFull game = and $ fmap T.isFull $ boards game

-- Makes a move on the current board or on any of the boards if the current board is any
-- If the current board is any then the indexes should be between 0 and board_size*board_size, otherwise they should be less than board_size and correspond to a position on the current board
move :: Int -> Int -> Piece -> GeniusTicTacToe -> GeniusTicTacToe
move rowIndex colIndex piece game
    | isNothing oldWinner =
        let (current, rowIndex', colIndex') = localCoordinates rowIndex colIndex game
        in game
    where
        oldWinner = winner game
        localCoordinates = (\rowIndex colIndex game ->
            case currentBoard game of
                Any -> 
                    let cBoard = board (rowIndex `quot` board_size) (colIndex `quot` board_size) game
                        rowIndex' = rowIndex `mod` board_size
                        colIndex' = colIndex `mod` board_size
                    in (cBoard, rowIndex', colIndex')
                Board boardIndex ->
                    (S.index (boards game) boardIndex, rowIndex, colIndex)
            )

