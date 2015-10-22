module TextFormat (
    format
) where

import qualified Genius as G
import qualified TicTacToe as T

import Data.List (intersperse)

import Genius (GeniusTicTacToe, Boards, currentBoard, board_size, boards, currentBoard)
import TicTacToe (TicTacToe, Pieces)

format :: GeniusTicTacToe -> String
format game = unlines . map (\i -> formatBoards . G.row i game) [1..board_size]

formatBoards :: Boards -> String
formatBoards gameBoards = unlines . map (\i -> formatBoardRow i) [1..board_size]
    where formatBoardRow index = concatMap (\board -> formatRow index board) gameBoards

formatRow :: Int -> TicTacToe -> String
formatRow n board =
    let pieces = map show $ T.row n board
    in " | " ++ intersperse " " pieces ++ " | "

