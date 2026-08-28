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

data GameState = Playing | Tie | Winner Sign | Start
data Game = Game {gameBoard :: Board, gameState :: GameState}

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

updateGameState :: Board -> GameState
updateGameState [a, b, c, d, e, f, g, h, i]
    | any (all (== X)) winnableRows = Winner X
    | any (all (== O)) winnableRows = Winner O
    | E `notElem` [a, b, c, d, e, f, g, h, i] = Tie
    | otherwise = Playing
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

readMoveToPos :: IO Int
readMoveToPos = do
    putStrLn "Enter the column you'd like to put your sign on: [1, 2, or 3]"
    col <- getLine
    putStrLn "Enter the row you'd like to put your sign on: [1, 2, or 3]"
    row <- getLine

    case (readMaybe col, readMaybe row) of
        (Just colI, Just rowI)
            | colI `elem` [1, 2, 3]
                && rowI `elem` [1, 2, 3] ->
                pure ((rowI - 1) * 3 + (colI - 1))
        _ -> do
            putStrLn "Invalid Input"
            readMoveToPos

moveIsValid :: Int -> Board -> Bool
moveIsValid index board = board !! index == E

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
        , ""
        , "To make a play enter the coordinates of the cell"
        , "you'd like to occupy"
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
            pos <- readMoveToPos

            if not (moveIsValid pos board)
                then do
                    putStrLn "That cell is taken!!"
                    playGame board sign
                else do
                    let newBoard = applyMove board pos sign
                        newSign = swapSign sign

                    playGame newBoard newSign

main :: IO ()
main = do
    let board = makeBoard
    putStrLn gameIntro
    playGame board X
