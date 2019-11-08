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


--this should be showBoard, call putStrLn in GHCI to test
showBoard :: Game -> String --One string per row assume the board is sorted
showBoard game = 
    let noPlaySpace = "~~~|"
        emptySpace =  "   |"
        regRedSpace = " r |"
        kingRedSpace =" R |"
        regBlkSpace = " b |"
        kingBlkSpace =" B |"
        seperator =   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
        columnLabels ="      1   2   3   4   5   6   7   8\n"
        top = columnLabels ++ seperator
        showRows::Board -> Int -> String
        showRows board row
            | row >= 9 = []
            | otherwise  = (showRow board 1 row 1)++(showRows board (row + 1))
    in show (top ++ (showRows (fst game) 1) ++ seperator) --i think the probloem is here

showRow :: Board -> Int -> Int -> Int -> String
showRow board smallerRoe c g = "ye\n"

{-
printRow :: Board -> Int -> Int -> Int -> String
printRow board smallerRow row column =
    if (not ((row,column) `elem` validSpaces))
    then noPlaySpace ++ 
    else if (smallerRow != 2) 
         then emptySpace ++
         else 
-}

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


--Doesn't work for strings like "1,"
readLoc :: String -> Maybe Loc --this should return a maybe loc in the future 3,7
readLoc str = 
    let remSpace :: [Char] -> String
        remSpace [] = []
        remSpace (x:xs) = 
            if x == ' '
            then remSpace xs
            else x : remSpace xs
    in  case (splitOn "," (remSpace str)) of
         --(x:[]) -> Nothing
         (x:y:[]) ->
            if (length x == 1 && length y == 1)
            then if (isDigit (head x) && isDigit (head y))
                 then Just ((read x :: Int),(read y :: Int))
                 else Nothing
            else Nothing
         --(x:[]) -> Nothing does not fix "1," string issue
         lst -> Nothing

