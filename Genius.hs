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
import Data.Matrix (Matrix)
import Data.Vector (Vector)

import qualified Data.Matrix as M

import TicTacToe (TicTacToe, Piece (PieceX, PieceO), board_size)

import qualified TicTacToe as T

data CurrentBoard = Any | Board (Int, Int) deriving (Show, Eq)
type BoardMatrix = Matrix TicTacToe
type Boards = Vector TicTacToe

data GeniusTicTacToe = GeniusTicTacToe {
    currentBoard :: CurrentBoard,
    boards :: BoardMatrix
} deriving (Show)

-- Create a new game
empty :: GeniusTicTacToe
empty = GeniusTicTacToe {
    boards=M.matrix board_size board_size $ \(i, j) -> T.empty,
    currentBoard=Any
}

-- Extracts a row of boards from the game
row :: Int -> GeniusTicTacToe -> Boards
row i game = M.getRow i $ boards game
-- Extracts a column of boards from the game
col :: Int -> GeniusTicTacToe -> Boards
col i game = M.getCol i $ boards game
-- Extracts a single board from the game
board :: Int -> Int -> GeniusTicTacToe -> TicTacToe
board row col game = M.getElem row col $ boards game

-- Returns whether the board is full
isFull :: GeniusTicTacToe -> Bool
isFull game = and $ fmap T.isFull $ M.toList $ boards game

-- Makes a move on the current board or on any of the boards if the current board is any
-- If the current board is any then the indexes should be between 0 and board_size*board_size, otherwise they should be less than board_size and correspond to a position on the current board
move :: Int -> Int -> Piece -> GeniusTicTacToe -> GeniusTicTacToe
move rowIndex colIndex piece game =
    let (current', rowIndex', colIndex') =
            if current == Any then localCoordinates rowIndex colIndex
            else (current, rowIndex, colIndex)
    in GeniusTicTacToe {
        currentBoard=Board (rowIndex', colIndex'),
        boards=moveBoard rowIndex' colIndex' piece game current'
    }
    where
        current = currentBoard game
        localCoordinates r c = (Board ((r-1) `quot` board_size + 1, (c-1) `quot` board_size + 1), (r-1) `mod` board_size + 1, (c-1) `mod` board_size + 1)

-- Makes the actual move and returns the new BoardMatrix
moveBoard :: Int -> Int -> Piece -> GeniusTicTacToe -> CurrentBoard -> BoardMatrix
moveBoard rowIndex colIndex piece game (Board (currentRow, currentCol))
    = M.setElem newBoard (currentRow, currentCol) (boards game)
    where newBoard = T.move rowIndex colIndex piece (board currentRow currentCol game)

-- Retrieves the winner of the board or Nothing
winner :: GeniusTicTacToe -> Maybe Piece
winner game = foldl1 (\acc w -> if isNothing acc then w else acc) winners
    where winners = map T.winner $ M.toList (boards game)

