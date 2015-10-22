module TextFormat (
    format,
    formatCurrentBoard
) where

import qualified Genius as G
import qualified TicTacToe as T

import Data.Maybe (isNothing, fromMaybe)
import Data.List (intersperse)
import Data.Vector (toList)

import Genius (GeniusTicTacToe, Boards, CurrentBoard (Any, Board), currentBoard, board_size, boards, currentBoard)
import TicTacToe (TicTacToe, Pieces, Piece)

format :: GeniusTicTacToe -> String
format game = boardHorizontalSeparator ++ "\n" ++ (unlines $ map (\i -> formatBoards $ G.row i game) [1..board_size])

formatBoards :: Boards -> String
formatBoards gameBoards = (unlines $ map (\i -> formatBoardRow i) [1..board_size]) ++ boardHorizontalSeparator
    where formatBoardRow index = concatMap (\board -> formatRow index board) gameBoards ++ " \x2551"

formatRow :: Int -> TicTacToe -> String
formatRow n board =
    let pieces = map (\p -> formatPiece p) $ toList $ T.row n board
    in " \x2551  " ++ (concat $ intersperse "  |  " pieces)

formatPiece :: Maybe Piece -> String
formatPiece Nothing = " "
formatPiece (Just p) = show p

boardHorizontalSeparator :: String
boardHorizontalSeparator = " " ++ (take 52 $ repeat '\x2550')

-- Completes the sentence "You can move on "
formatCurrentBoard :: CurrentBoard -> String
formatCurrentBoard Any = "any"
formatCurrentBoard (Board (row, col)) = "the "
    ++ (["top", "middle", "bottom"] !! (row - 1))
    ++ " "
    ++ (["left", "center", "right"] !! (col - 1))
