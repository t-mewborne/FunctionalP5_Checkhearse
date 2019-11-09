module Checkhearse where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.List.Split
import Data.String
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))
import Data.Char

data Player = Black | Red deriving Show
data Piece = Reg Player | King Player | Empty deriving Show


type Loc = (Int,Int) --(row,column)
type Square = (Loc,Piece)
--type Board = [[Piece]]
type Board = [Square] --choose between these two types for board (only contains playable spaces)
type Game = (Board,Player) --player = current turn

-- type Board = [Square] (2nd Declare)
type Move = ((Char,Int),(Char,Int)) --((Start),(End),Turn)

buildGame :: Game --Initial State of the board
buildGame = 
    let bd = buildRow 1 2 (Reg Black) ++
             buildRow 2 1 (Reg Black) ++
             buildRow 3 2 (Reg Black) ++
             buildRow 4 1 Empty ++
             buildRow 5 2 Empty ++
             buildRow 6 1 (Reg Red) ++
             buildRow 7 2 (Reg Red) ++
             buildRow 8 1 (Reg Red)
        buildRow :: Int -> Int -> Piece -> Board --build one row
        buildRow row column piece
            | column >= 9 = []
            | otherwise = ((row,column),piece):(buildRow row (column + 2) piece)
    in (bd, Black)


validSpaces :: [Loc]
validSpaces = [(x,y) | x <- [1..8], y <- [1..8], (even x && odd y) || (odd x && even y)]

--call putStrLn in GHCI to test (putStrLn $ showBoard buildGame)
showBoard :: Game -> String --One string per row assume the board is sorted
showBoard game = 
    let seperator =   "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
        columnLabels ="\n       1     2     3     4     5     6     7     8\n"
        top = columnLabels ++ seperator
        showRows::Board -> Int -> String
        showRows board row
            | row >= 9 = []
            | otherwise  = (showRow board row) ++ seperator ++ (showRows board (row + 1))
        showTurn = 
            case snd game of
            Black -> "\n\x1b[30m Player 1's turn (Black Pieces)\x1b[0m\n"
            Red -> "\n\x1b[31m Player 2's turn (Red Pieces)\x1b[0m\n"
    in top ++ (showRows (fst game) 1) ++ showTurn

showRow :: Board -> Int -> String
showRow board row = 
    let noPlaySpace = "~~~~~|"
        emptySpace =  "     |"
        rowNumSpace = " " ++ show row ++ " |"
        --next = aux smallRow (column + 1)
        aux :: Int -> Int -> String
        aux 4 column = []
        aux smallRow 9 = '\n' : (aux (smallRow + 1) 0)
        aux smallRow column =
            (if ((row,column) `elem` validSpaces)
            then if (smallRow == 2) 
                 then showSpace board (row,column)
                 else emptySpace
            else case (smallRow,column) of
                    (2,0) -> " " ++ rowNumSpace
                    (_,0) -> "    |"
                    _ -> noPlaySpace)
            ++ aux smallRow (column + 1)
    in aux 1 0

showSpace :: Board -> Loc -> String
showSpace (space:spaces) loc =
    let clrRed = "\x1b[31m"
        clrBlk = "\x1b[30m"
        clrRst = "\x1b[0m"
        showPiece = 
            case snd space of
                Reg Red    -> clrRed ++ "  r  " ++ clrRst ++ "|"
                Reg Black  -> clrBlk ++ "  b  " ++ clrRst ++ "|"
                King Red   -> clrRed ++ "  R  " ++ clrRst ++ "|"
                King Black -> clrBlk ++ "  B  " ++ clrRst ++ "|"
                Empty -> "     |"
    in  if ((fst space) == loc)
        then showPiece
        else if ((length spaces) <= 0) 
             then error "Space " ++ show loc ++ " not found"
             else showSpace spaces loc


{-
     1   2   3
   ~~~~~~~~~~~~~
   |~~~|   |~~~|
 1 |~~~| R |~~~|
   |~~~|   |~~~|
   ~~~~~~~~~~~~~
   |   |~~~|   |
 2 |   |~~~| r |
   |   |~~~|   |
   ~~~~~~~~~~~~~
-}

updateBoard :: Game -> Move -> Maybe Board --is the move valid? Should we make this a return a Maybe Game?
updateBoard = undefined

validMove :: Game -> Move -> Bool
validMove = undefined

readLoc :: String -> Maybe Loc
readLoc str = 
    let remSpace :: [Char] -> String
        remSpace [] = []
        remSpace (x:xs) = 
            if x == ' '
            then remSpace xs
            else x : remSpace xs
    in  case (splitOn "," (remSpace str)) of
         (x:y:[]) ->
            if (length x == 1 && length y == 1)
            then if (isDigit (head x) && isDigit (head y))
                 then Just ((read x :: Int),(read y :: Int))
                 else Nothing
            else Nothing
         lst -> Nothing

