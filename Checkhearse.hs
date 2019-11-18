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
type Board = [Square] --choose between these two types for board (only contains playable spaces)
type Game = (Board,Player) --player = current turn
type Move = (Loc,Loc) --((Start),(End),Turn)
data Outcome = Won Player | Tie deriving (Eq, Show)

outOf :: Integer -> Integer -> Rational
outOf a b = (fromIntegral a) % (fromIntegral b)

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
        buildRow :: Int -> Int -> Piece -> Board --build one single row
        buildRow row column piece
            | column >= 9 = []
            | otherwise = ((row,column),piece):(buildRow row (column + 2) piece)
    in (bd, Black)


validSpaces :: [Loc]
validSpaces = [(x,y) | x <- [1..8], y <- [1..8], (even x && odd y) || (odd x && even y)]

--call putStrLn in GHCI to test (putStrLn $ showBoard buildGame)
showBoard :: Game -> String
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
      1     2     3
   ~~~~~~~~~~~~~~~~~~~
   |~~~~~|     |~~~~~|
 1 |~~~~~|  R  |~~~~~|
   |~~~~~|     |~~~~~|
   ~~~~~~~~~~~~~~~~~~~
   |     |~~~~~|     |
 2 |     |~~~~~|  r  |
   |     |~~~~~|     |
   ~~~~~~~~~~~~~~~~~~~
-}



updateBoard :: Game -> Move -> Maybe Game
updateBoard game ((r1, c1), (r2, c2)) =
  if not (validMove game ((r1, c1), (r2, c2))) then Nothing --- SAME GAME, BC INVALID MOVE
  else if abs (r2-r1)==1
    then makeScoot game ((r1, c1), (r2, c2))  
  else makeJump game ((r1, c1), (r2, c2))  


  -- first check validMove
  -- determine if move is jump or scoot & do validJump or validScoot
  -- if |y2 - y1| = 1, then scoot
  -- if |y2 - y1| > 1, then jump

{-
makeJump :: Game -> Move -> Maybe Game
makeJump (bd, plyr) ((y1, x1), (y2, x2)) 
  | victim == Nothing                    = Nothing --error tryna jump off board
  | victim == Empty                           = Nothing --error can't jump empty space
  | victim == King plyr || victim == Reg plyr = Nothing --error can't jump self
  | otherwise                                 = doJump (y1, x1) (y2, x2) (xv, yv) bd plyr victim -- slap a Just there
  where 
  -}

makeJump :: Game -> Move -> Maybe Game
makeJump (bd, plyr) ((r1, c1), (r2, c2)) =
    do let cv = if (c2-c1>0) -- to the right
             then c1+1
             else c1-1 -- to the left
       let rv = if (r2-r1>0) -- to the bottom
             then r1+1
             else r1-1 -- to the top
       victim <- lookup (rv, cv) bd
       if (victim `elem` [Reg plyr, King plyr, Empty])
       then Nothing
       else Just $ doJump (r1, c1) (r2, c2) ((rv,cv),victim) (bd,plyr)

-- returns boolean that tells if it's a valid jump & square of the victim
-- if false or a scoot, then returns ((0,0), Reg Red)
checkForJump :: Game -> Move -> Bool--(Bool, Square)
checkForJump (bd, plyr) ((r1,c1), (r2,c2)) =
   let cv = if (c2-c1>0) -- to the right
             then c1+1
             else c1-1 -- to the left
       rv = if (r2-r1>0) -- to the bottom
             then r1+1
             else r1-1
       victim = 
            case lookup (rv, cv) bd of
                    Nothing -> Empty
                    Just v -> v
   in if abs (r2-r1)==1 -- if it's a scoot, it returns true
      then True --(True, ((0,0), Reg Red))
      else if (victim `elem` [Reg plyr, King plyr, Empty]) -- if jumping over own piece, then false
      then False --(False, ((0,0), Reg Red))
      else True --(True, ((rv, cv), victim))


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

doJump :: Loc -> Loc -> Square -> Game -> Game
doJump start end victim (bd,plyr) =
    let activePiece =
            case lookup start bd of
                Just piece -> piece
                _ -> error "(doJump) Invalid start piece"
        remStart = setPiece bd (start, activePiece) Empty --Probably should remove this when we remove empties
        remVictim = setPiece remStart victim Empty
        nextTurn = otherPlayer plyr
    in  (setPiece remVictim (end, Empty) activePiece, nextTurn)
--  in checkMoreJumps plyr (x2,y2) (setPiece bdWoutVictim ((x2, y2), Empty) activePiece, nextTurn)
-- ^^ this passes player, the location, and the board with the jump made & will check for more jumps

otherPlayer :: Player -> Player
otherPlayer Red = Black
otherPlayer Black = Red

makeScoot :: Game -> Move -> Maybe Game
makeScoot (bd, plyr) (start, end) =
  let activePiece = 
        case lookup start bd of
            Just piece -> piece
            _ -> error "(makeScoot) Invalid start piece"
      remStart = setPiece bd (start, activePiece) Empty
      nextTurn = otherPlayer plyr
  in Just (setPiece remStart (end,Empty) activePiece, nextTurn)


-- will change into 2 funcs when we get rid of empties
-- create a remove/add functions
setPiece :: Board -> Square -> Piece -> Board
setPiece bd (loc,oldPiece) replacement = [ if x==loc then (loc,replacement) else (x,y) | (x,y) <- bd]

{-
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
  where start = lookup (x1,y1) bd
        end = lookup (x2,y2) bd
-}

validMove :: Game -> Move -> Bool
validMove (bd,plyr) (startLoc,endLoc) =
    let startPc = lookup startLoc bd
        endPc = lookup endLoc bd
    in  case (startPc,endPc) of
            (Just start,Just Empty) -> valPlyr start plyr && rightDir (startLoc,start) (endLoc,Empty) 
                                       && checkForJump (bd, plyr) (startLoc, endLoc)--change "empty" to "nothing"
            _ -> False

{-
-- Tested and Works
rightDir :: Loc -> Maybe Piece -> Loc ->  Maybe Piece -> Player -> Bool
rightDir (y1,x1) (Just (King color)) (y2,x2) (Just Empty) plyr = True
rightDir (y1,x1) (Just (Reg color)) (y2,x2) (Just Empty) plyr
  | (color == Black && (y2-y1>0))= True
  | (color == Red && (y2-y1<0))  = True
  | otherwise                    = False
-}

--Makes sure you are moving in the right direction based on piece and its color
rightDir :: Square -> Square -> Bool
rightDir ((y1,x1),King _) ((y2,x2),Empty) = True
rightDir ((y1,x1),Reg color) ((y2,x2),Empty) = (color == Black && (y2-y1>0)) || (color == Red && (y2-y1<0))


--returns whether or not a piece is a player's piece
{-
valPlyr :: Maybe Piece -> Player -> Bool   --This should not take in a Maybe Piece!!
valPlyr (Just (Reg color)) turn = (color == turn)
valPlyr (Just (King color)) turn = (color == turn)
valPlyr _ = False
-}

valPlyr :: Piece -> Player -> Bool
valPlyr (King plyr) turn = (plyr == turn)
valPlyr (Reg  plyr) turn = (plyr == turn)

-- will implement a counter that cuts game at certain point
winner :: Game -> Maybe Outcome
winner (board, p)
    | nobodyWins = Just Tie
    | redWins = Just $ Won Red
    | blackWins = Just $ Won Black
    | otherwise = Nothing
    where nobodyWins = all (\square -> (snd square) == Empty) board
          redWins = all (\square -> (snd square) == (Reg Red)  ||
                                  (snd square) == (King Red) ||
                                  (snd square) == (Empty)) board
          blackWins = all (\square -> (snd square) == (Reg Black)  ||
                                    (snd square) == (King Black) ||
                                    (snd square) == (Empty)) board

-- means current player can force a win, that move exists
willWin :: Game -> Outcome
willWin (bd, plyr) = 
  let possGames = allGames (bd, plyr)
      outcomes = [winner gm | gm <- possGames, w=winner gm]
--
  in if (Won plyr `elem` outcomes)
     then Won plyr
     else if (Tie `elem` outcomes)
     then Tie
     else Won $ otherPlayer player

validMoves :: Game -> [Move]
validMoves (bd, plyer) =
  let plSquare = [(loc, pc) | (loc, pc) <- bd, (pc==Reg plyer || pc ==King plyer)]
      mvsForSquare ((r, c), pc) = [((r,c), l) | l <- [(r+1, c+1), (r+1, c-1), (r-1, c-1), (r-1, c+1),
                                                      (r+2, c+2), (r+2, c-2), (r-2, c-2), (r-2, c+2)]]
      mvsForPlayer [] = []
      mvsForPlayer (x:xs) = mvsForSquare x++ mvsForPlayer xs
  in [mv | mv <- mvsForPlayer plSquare, validMove (bd, plyer) mv]


bestMove :: Game -> Move
bestMove (bd, plyr) =
  let allMoves = validMoves (bd, plyr)
      
      aux :: Game -> Player -> Move
      

bestMove :: Game -> Move
bestMove (bd, plyr) =
  let allMoves = validMoves (bd, plyr)
      assocMvPercent = [((bestMvRec (updateBoard (bd, plyr) mv) plyr), mv) | mv <- allMoves]
  in snd $ maximum assocMvPercent

bestMvRec :: Maybe Game -> Player -> Rational
bestMvRec maybeGm turn =
  let gm = case maybeGm of
             Just g -> g
             Nothing -> error "bestMvRec got invalid game"
      lst = aux gm turn
      (bd, p) = gm
      aux :: Game -> Player -> [Integer]
      aux gm plyr =
        case winner bd of 
          Nothing -> [bestMvRec g turn | g <- allGames gm]
          Just turn -> 1
          Just _ -> 0
  
--  in (sum lst) `outOf` (length lst)
  

-- gives list of possible gamestates
allGames (bd, plyer) =
  let allMoves = validMoves (bd, plyer)
  in [updateBoard (bd, plyer) mv | mv <- allMoves]
-- things to take into consideration:
--   1) get pnts
--   2) dont get killed:
--      i) dont move to spot that gets you killed
--      ii) mv a defending pc to help a pc about to get killed
--   3) make a king pc

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

