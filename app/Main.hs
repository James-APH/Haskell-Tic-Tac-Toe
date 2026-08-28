module Main (main) where

data Sign = X | O | E
    deriving (Eq)

newtype Tile = Tile {tSign :: Sign}

data Row = Row Tile Tile Tile

data Board = Board Row Row Row

instance Show Sign where
    show X = " X "
    show O = " O "
    show E = "   "

data GameState = Playing | Tie | Winner Sign | Start

data Game = Game {gameBoard :: Board, gameState :: GameState}

tileInit :: Tile
tileInit = Tile E

showSign :: Tile -> String
showSign tile = show (tSign tile)

rowInit :: Row
rowInit = Row tileInit tileInit tileInit

getSigns :: Row -> [Sign]
getSigns (Row t1 t2 t3) = map tSign [t1, t2, t3]

boardInit :: Board
boardInit = Board rowInit rowInit rowInit

renderRow :: Row -> String
renderRow (Row t1 t2 t3) =
    showSign t1
        ++ "|"
        ++ showSign t2
        ++ "|"
        ++ showSign t3
        ++ "\n"

renderBoard :: Board -> String
renderBoard (Board r1 r2 r3) =
    "    1   2   3 \n"
        ++ " 1 "
        ++ renderRow r1
        ++ "   ---+---+---\n"
        ++ " 2 "
        ++ renderRow r2
        ++ "   ---+---+---\n"
        ++ " 3 "
        ++ renderRow r3

updateGameState :: Board -> GameState
updateGameState (Board r1 r2 r3)
    | any (all (== X)) winnableRows = Winner X
    | any (all (== O)) winnableRows = Winner O
    | all (/= E) [a, b, c, d, e, f, g, h, i] = Tie
    | otherwise = Playing
  where
    [a, b, c] = getSigns r1
    [d, e, f] = getSigns r2
    [g, h, i] = getSigns r3
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

-- updateBoardState

-- updateGame

main :: IO ()
main = putStrLn (renderBoard boardInit)
