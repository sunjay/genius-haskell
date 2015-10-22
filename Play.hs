import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe (isNothing)

import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)

import Genius (GeniusTicTacToe, empty, move, Piece (PieceX, PieceO))

main = do
    hSetBuffering stdout NoBuffering
    playGame PieceX empty

playGame :: Piece -> GeniusTicTacToe -> IO ()
playGame currentPiece game = do
    print game
    putStrLn $ "It is currently " ++ (show currentPiece) ++ "'s move"
    (row, col) <- getValidMove
    let game' = move row col currentPiece game
        currentPiece' = if currentPiece == PieceX then PieceO else PieceX
    playGame currentPiece' game'

getValidMove :: IO (Int, Int)
getValidMove = do
    putStr "Enter your move: "
    parsedMove <- fmap parseInput getLine
    if isNothing parsedMove then do
        putStrLn "Please enter a valid move in the form A1"
        getValidMove
    else
        let Just (row, col) = parsedMove
        in return (row, col)


parseInput :: String -> Maybe (Int, Int)
parseInput string
    | length string == 2 =
        let col = (+1) <$> elemIndex (toUpper (string !! 0)) ['A'..]
            row = (+1) <$> elemIndex (string !! 1) ['1'..'9']
        in if isNothing col || isNothing row then
                Nothing
            else
                (,) <$> row <*> col
    | otherwise = Nothing
