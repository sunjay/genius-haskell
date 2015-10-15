import Data.Char (toUpper, digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import Genius (GeniusTicTacToe, empty, move)

main = playGame empty

playGame :: GeniusTicTacToe -> IO ()
playGame game = do
    rawMove <- getLine
    let (col, row) = parseInput rawMove
    print col
    print row

parseInput :: String -> (Int, Int)
parseInput string
    | length string == 2 =
        let col = (fromMaybe 
                (error "Invalid column letter")
                (elemIndex (toUpper (string !! 0)) ['A'..])) + 1
            row = digitToInt $ string !! 1
        in (row, col)
    | otherwise = error "Enter a move in the form A1"


