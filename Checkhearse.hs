module Checkhearse where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))

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
    let bd = buildRow 1 2 Black ++
             buildRow 2 1 Black ++
             buildRow 3 2 Black ++
             buildRow 4 1 Empty ++
             buildRow 5 2 Empty ++
             buildRow 6 1 Red ++
             buildRow 7 2 Red ++
             buildRow 8 1 Red
        buildRow :: Int -> Int -> Player -> Board --build one row
        buildRow row column plyr
            | column >= 9 = []
            | otherwise = ((row,column),Reg plyr):(buildRow row (column + 2) plyr)
    in (bd, Black)

showBoard :: Game ->  [String] --One string per row
showBoard game = undefined

updateBoard :: Game -> Move -> Maybe Board --is the move valid? Should we make this a return a Maybe Game?
updateBoard = undefined

validMove :: Game -> Move -> Bool
validMove = undefined

readLoc :: String -> Loc --this should return a maybe loc in the future
readLoc = undefined
