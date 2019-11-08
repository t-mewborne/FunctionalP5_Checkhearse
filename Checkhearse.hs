module Checkhearse where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))

data Player = Black | Red deriving (Eq,Show)
data Piece = Reg Player | King Player | Empty deriving (Eq,Show)


type Loc = (Int,Int) --(row,column)
type Square = (Loc,Piece)
--type Board = [[Piece]]
type Board = [Square] --choose between these two types for board (only contains playable spaces)
type Game = (Board,Player) --player = current turn

-- type Board = [Square] (2nd Declare)
data Move = Jump (Loc,Loc) | Scoot (Loc,Loc) --((Start),(End),Turn)

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
{-
showBoard :: Game ->  [String] --One string per row
showBoard (board,player):game = 
    case board of
        ((x,y),piece) = 
-}



updateBoard :: Game -> Move -> Maybe Game
updateBoard = undefined
  -- first check validMove
  -- determine if move is jump or scoot & do validJump or validScoot
  -- if |y2 - y1| = 1, then scoot
  -- if |y2 - y1| > 1, then jump

--movalidMove :: Game -> Move -> Maybe Bool
--movalidMove (bd, plyr) ((x1, y1), (x2, y2)) =
--    do srt <- lookup (x1,y1) bd
--       end <- lookup (x2,y2) bd

validJump :: Game -> Move -> Bool
validJump (bd, plyr) (Jump ((x1, y1), (x2, y2))) = undefined
--  determine if theres a piece/ are pieces bt start & end
-- tells if that piece / pieces are the other player's

validScoot :: Game -> Move -> Bool
validScoot (bd, plyr) (Scoot ((x1, y1), (x2, y2))) = undefined
--

-- validMove checks if start & end are on the board & if start is player's color
validMove :: Game -> Move -> Bool
validMove (bd, plyr) ((x1, y1), (x2, y2))
     | (start == Nothing)              = False
     | (end == Nothing)                = False
     | (end /= Just Empty)             = False
     | (not (valPlyr start plyr))      = False
     | (not (rightDir start end plyr)) = False
     | otherwise                       = True
  where start = pAtLoc (x1,y1) bd
        end = pAtLoc (x2,y2) bd

rightDir :: Maybe Square -> Maybe Square -> Player -> Bool
rightDir (Just ((x1, y1), King color)) (Just ((x2, y2), Empty)) plyr = True
rightDir (Just ((x1, y1), Reg color)) (Just ((x2, y2), Empty)) plyr
  | (color == Black && (y2-y1>0))= True
  | (color == Red && (y2-y1<0))  = True
  | otherwise                    = False

--valPlyr :: Maybe Piece -> Player -> Bool  
valPlyr (Just (Reg color)) turn = color == turn
valPlyr (Just (King color)) turn = color == turn

pAtLoc :: Loc -> Board -> Piece
pAtLoc loc bd = lookup loc bd


readLoc :: String -> Loc --this should return a maybe loc in the future
readLoc = undefined
