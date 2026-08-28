module Main (main) where

import Text.Read (readMaybe)

data Sign = X | O | E
    deriving (Eq)

instance Show Sign where
    show X = " X "
    show O = " O "
    show E = "   "

swapSign :: Sign -> Sign
swapSign X = O
swapSign O = X
swapSign _ = E

type Board = [Sign]
data GameStatus = Tie | Winner Sign | GameInProgress

makeBoard :: Board
makeBoard = replicate 9 E

renderBoard :: Board -> String
renderBoard [a, b, c, d, e, f, g, h, i] =
    unlines
        [ "    1   2   3"
        , " 1 " ++ show a ++ "|" ++ show b ++ "|" ++ show c
        , "   ---+---+---"
        , " 2 " ++ show d ++ "|" ++ show e ++ "|" ++ show f
        , "   ---+---+---"
        , " 3 " ++ show g ++ "|" ++ show h ++ "|" ++ show i
        ]

updateGameState :: Board -> GameStatus
updateGameState [a, b, c, d, e, f, g, h, i]
    | any (all (== X)) winnableRows = Winner X
    | any (all (== O)) winnableRows = Winner O
    | E `notElem` [a, b, c, d, e, f, g, h, i] = Tie
    | otherwise = GameInProgress
  where
    winnableRows =
        [ [a, b, c]
        , [d, e, f]
        , [g, h, i]
        , [a, d, g]
        , [b, e, h]
        , [c, f, i]
        , [a, e, i]
        , [c, e, g]
        ]

parseStringToIndex :: String -> Int
parseStringToIndex (row : ' ' : col : _) =
    case (readMaybe row, readMaybe col) of
        (Just ri, Just ci)
            | ri `elem` [1, 2, 3]
                && ci `elem` [1, 2, 3] ->
                (ri - 1) * 3 + (ci - 1)
        _ -> -1
parseStringToIndex _ = -1

readMoveIndex :: IO Int
readMoveIndex = do
    putStrLn "Enter the row & column (1-3) like \"# #\" that you'd like to put your sign on:"
    rowCol <- getLine
    let parsedIndex = parseStringToIndex rowCol
    if parsedIndex < 0
        then do
            putStrLn "Invalid Input"
            readMoveIndex
        else
            pure parsedIndex

applyMove :: Board -> Int -> Sign -> Board
applyMove board index sign =
    take index board ++ [sign] ++ drop (index + 1) board

gameIntro :: String
gameIntro =
    unlines
        [ "Welcome to tic-tac-toe, a game about X's and O's"
        , "Win by getting three of your sign [X/O] in a row"
        , "Rows can be vertical, horizontal or diagonal"
        , "The game can only end in victory or a tie"
        ]

playGame :: Board -> Sign -> IO ()
playGame board sign = do
    putStrLn (renderBoard board)
    case updateGameState board of
        Winner X -> putStrLn "X is the WINNER!!!"
        Winner O -> putStrLn "O is the WINNER!!!"
        Tie -> putStrLn "No winners (Tie)"
        _ -> do
            putStrLn (show sign ++ "'s turn:")
            index <- readMoveIndex
            if board !! index == E
                then do
                    putStrLn "That cell is taken!!"
                    playGame board sign
                else do
                    let newBoard = applyMove board index sign
                        newSign = swapSign sign
                    playGame newBoard newSign

main :: IO ()
main = do
    let board = makeBoard
    putStrLn gameIntro
    playGame board X
