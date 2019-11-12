module Checkhearse where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.List.Split
import Data.String
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))
import Data.Maybe
import Data.Char

data Player = Black | Red deriving (Eq,Show)
data Piece = Reg Player | King Player | Empty deriving (Eq,Show)


type Loc = (Int,Int) --(row,column)
type Square = (Loc,Piece)
--type Board = [[Piece]]
type Board = [Square] --choose between these two types for board (only contains playable spaces)
type Game = (Board,Player) --player = current turn

-- type Board = [Square] (2nd Declare)
type Move = (Loc,Loc) --((Start),(End),Turn)

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
            Black -> "\n\x1b[30m  Player 1's turn (Black Pieces)\x1b[0m\n"
            Red -> "\n\x1b[31m  Player 2's turn (Red Pieces)\x1b[0m\n" 
    in top ++ (showRows (fst game) 1) ++ showTurn

showRow :: Board -> Int -> String
showRow board row = 
    let noPlaySpace = "~~~~~|"
        emptySpace =  "     |"
        rowNumSpace = " " ++ show row ++ " |"
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
    let clrRed = "\x1b[31m" --change text color to red
        clrBlk = "\x1b[30m" --change text color to black
        clrRst = "\x1b[0m"  --reset  text color to default
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



updateBoard :: Game -> Move -> Game
updateBoard game ((x1, y1), (x2, y2)) =
  if not (validMove game ((x1, y1), (x2, y2))) then game --- SAME GAME, BC INVALID MOVE
  else if abs (y2-y1)==1
    then makeScoot game ((x1, y1), (x2, y2))  
  else makeJump game ((x1, y1), (x2, y2))  


  -- first check validMove
  -- determine if move is jump or scoot & do validJump or validScoot
  -- if |y2 - y1| = 1, then scoot
  -- if |y2 - y1| > 1, then jump

makeJump :: Game -> Move -> Game
makeJump (bd, plyr) ((y1, x1), (y2, x2)) 
  | maybeVictim == Nothing                    = (bd, plyr) --error tryna jump off board
  | victim == Empty                           = (bd, plyr) -- error can't jump empty space
  | victim == King plyr || victim == Reg plyr = (bd, plyr) -- error can't jump self
  | otherwise                                 = doJump (y1, x1) (y2, x2) (xv, yv) bd plyr victim
  where xv = if (x2-x1>0) -- to the right
             then x1+1
             else x1-1 -- to the left
        yv = if (y2-y1>0) -- to the bottom
             then y1+1
             else y1-1 -- to the top
        maybeVictim = pAtLoc (xv, yv) bd
        victim = fromJust maybeVictim

{- need function to do multiple jumps
 do we make it so that the user types each space they want to hit?
 or do they just type end placement & computer chooses which one?
 or does computer prompt user if theres another jump available?
-}


--This function will be called by doJump, if there's another jump available,
--it will call doJump & if there's not another jump, it'll return the game from doJump
{-checkMoreJumps :: Player -> Loc -> Board -> Game
  checkMoreJumps plyr loc board =
  let lftLoc = (,)
     rgtLoc = (,)
     rgtPiece = fndPiece rgtLoc board
     lftPiece = fndPiece lftLoc board
     (lftJumpResult, p) = makeJump (board, plyr) (loc, lftLoc)
     (rgtJumpResult, p) = makeJump (board, plyr) (loc, rgtLoc)
  in if (lftJumpResult /= board)
    then (lftJumpResult, --will figure out player part)
     else if (rgtJumpResult /= board)
     then (rgtJumpResult, --will figure out player part)
     else (board, --player)
-}
      
{-fndPiece loc board = 
  let maybePiece = pAtLoc loc board
  in if maybePiece == Nothing
     then Nothing
     else fromJust (maybePiece)
-}
{-      Just activePiece = pAtLoc(x1, y2) bd
        bdWoutStart = setPiece bd ((x1, y1), activePiece) Empty
        bdWoutVictim = setPiece bdWoutStart ((xv, yv), victim) Empty
        nextTurn = if plyr == Red
                   then Black
                   else Red
        ret = (setPiece bWoutVictim ((x2, y2), Empty) activePiece, nextTurn)
-}

--doJump Loc -> Loc -> Loc -> Board -> Player -> Piece -> Game
doJump (x1,y1) (x2, y2) (xv, yv) bd plyr victim =
  let Just activePiece = pAtLoc(x1, y2) bd
      bdWoutStart = setPiece bd ((x1, y1), activePiece) Empty
      bdWoutVictim = setPiece bdWoutStart ((xv, yv), victim) Empty
      nextTurn = if plyr == Red
                 then Black
                 else Red
  in (setPiece bdWoutVictim ((x2, y2), Empty) activePiece, nextTurn)
--  in checkMoreJumps plyr (x2,y2) (setPiece bdWoutVictim ((x2, y2), Empty) activePiece, nextTurn)
-- ^^ this passes player, the location, and the board with the jump made & will check for more jumps


makeScoot :: Game -> Move -> Game
makeScoot (bd, plyr) ((x1, y1), (x2, y2)) =
  let Just activePiece = pAtLoc (x1, y1) bd
      bdWoutStart = setPiece bd ((x1, y1), activePiece) Empty
      nextTurn = if plyr == Red
                 then Black
                 else Red
  in (setPiece bdWoutStart ((x2, y2),Empty) activePiece, nextTurn)

setPiece :: Board -> Square -> Piece -> Board
setPiece bd (loc,oldPiece) replacement = [ if x==loc then (loc,replacement) else (x,y) | (x,y) <- bd]

--removeAndInsert [fstPieces]:(loc,_):[lstPieces] loc replacement = 

-- validMove checks if start & end are on the board & if start is player's color
-- Tested and VERY CORRECT
validMove :: Game -> Move -> Bool
validMove (bd, plyr) ((x1, y1), (x2, y2))
     | (start == Nothing)                              = False -- start spot is playable spot on board
     | (end == Nothing)                                = False -- end spot is playable spot on board
     | (end /= Just Empty)                             = False -- make sure end spot is empty
     | (not (valPlyr start plyr))                      = False -- piece at start is player's whose turn it is
     | (not (rightDir (x1,y1) start (x2,y2) end plyr)) = False -- piece is moving right dir based on turn
     | otherwise                                       = True  -- is a valid general move
  where start = pAtLoc (x1,y1) bd
        end = pAtLoc (x2,y2) bd

-- Tested and Works
rightDir :: Loc -> Maybe Piece -> Loc ->  Maybe Piece -> Player -> Bool
rightDir (y1,x1) (Just (King color)) (y2,x2) (Just Empty) plyr = True
rightDir (y1,x1) (Just (Reg color)) (y2,x2) (Just Empty) plyr
  | (color == Black && (y2-y1>0))= True
  | (color == Red && (y2-y1<0))  = True
  | otherwise                    = False


valPlyr :: Maybe Piece -> Player -> Bool  
valPlyr (Just (Reg color)) turn = (color == turn)
valPlyr (Just (King color)) turn = (color == turn)

pAtLoc :: Loc -> Board -> Maybe Piece
pAtLoc loc bd = lookup loc bd

winner :: Board -> Maybe Player
winner board
    | nobodyWins = Nothing
    | redWins = Just Red
    | blackWins = Just Black
    | otherwise = Nothing
    where nobodyWins = all (\square -> (snd square) == Empty) board
          redWins = all (\square -> (snd square) == (Reg Red)  ||
                                  (snd square) == (King Red) ||
                                  (snd square) == (Empty)) board
          blackWins = all (\square -> (snd square) == (Reg Black)  ||
                                    (snd square) == (King Black) ||
                                    (snd square) == (Empty)) board

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

