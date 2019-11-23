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
import Debug.Trace

data Player = Black | Red deriving (Eq,Show)
data Piece = Reg Player | King Player | Empty deriving (Eq,Show)
data Outcome = Won Player | Tie deriving (Eq, Show)

type Loc = (Int,Int) --(row,column)
type Square = (Loc,Piece)
type Board = [Square]
type Game = (Board,Player,Int) --the player it the current turn, the int is the count
type Move = (Loc,Loc) --((Start),(End),Turn)

buildGame :: Int -> Game --Initial State of the board, DO NOT USE A COUNT OVER 75
buildGame count = 
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
    in (bd, Black,count)

validSpaces :: [Loc]
validSpaces = [(x,y) | x <- [1..8], y <- [1..8], (even x && odd y) || (odd x && even y)]

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

makeJump :: Game -> Move -> Maybe Game
makeJump (bd, plyr, count) ((r1, c1), (r2, c2)) =
    do let cv = if (c2-c1>0) -- to the right
             then c1+1
             else c1-1 -- to the left
       let rv = if (r2-r1>0) -- to the bottom
             then r1+1
             else r1-1 -- to the top
       victim <- lookup (rv, cv) bd
       if (victim `elem` [Reg plyr, King plyr, Empty])
       then Nothing
       else Just $ doJump (r1, c1) (r2, c2) ((rv,cv),victim) (bd,plyr,count)

-- returns boolean that tells if it's a valid jump & square of the victim
-- if false or a scoot, then returns ((0,0), Reg Red)
checkForJump :: Game -> Move -> Bool--(Bool, Square)
checkForJump (bd, plyr,count) ((r1,c1), (r2,c2)) =
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
doJump start end victim (bd,plyr,count) =
    let activePiece =
            case lookup start bd of
                Just piece -> piece
                _ -> error "(doJump) Invalid start piece"
        remStart = setPiece bd (start, activePiece) Empty --Probably should remove this when we remove empties
        remVictim = setPiece remStart victim Empty
        nextTurn = otherPlayer plyr
    in  (setPiece remVictim (end, Empty) activePiece, nextTurn, count-1)
--  in checkMoreJumps plyr (x2,y2) (setPiece bdWoutVictim ((x2, y2), Empty) activePiece, nextTurn)
-- ^^ this passes player, the location, and the board with the jump made & will check for more jumps

otherPlayer :: Player -> Player
otherPlayer Red = Black
otherPlayer Black = Red

makeScoot :: Game -> Move -> Maybe Game
makeScoot (bd, plyr,count) (start, end) =
  let activePiece = 
        case lookup start bd of
            Just piece -> piece
            _ -> error "(makeScoot) Invalid start piece"
      remStart = setPiece bd (start, activePiece) Empty
      nextTurn = otherPlayer plyr
  in Just (setPiece remStart (end,Empty) activePiece, nextTurn, count-1)

-- will change into 2 funcs when we get rid of empties
-- create a remove/add functions
setPiece :: Board -> Square -> Piece -> Board
setPiece bd (loc,oldPiece) replacement = [ if x==loc then (loc,replacement) else (x,y) | (x,y) <- bd]

--validMove checks if start & end are on the board & if start is player's color
validMove :: Game -> Move -> Bool
validMove (bd,plyr,count) (startLoc,endLoc) =
    let startPc = lookup startLoc bd
        endPc = lookup endLoc bd
    in  case (startPc,endPc) of
            (Just start,Just Empty) -> count > 0 && valPlyr start plyr && rightDir (startLoc,start) (endLoc,Empty) 
                                       && checkForJump (bd, plyr,count) (startLoc, endLoc)--change "empty" to "nothing"
            _ -> False

--Make sure you are moving in the right direction based on piece and its color
rightDir :: Square -> Square -> Bool
rightDir ((y1,x1),King _) ((y2,x2),Empty) = True
rightDir ((y1,x1),Reg color) ((y2,x2),Empty) = (color == Black && (y2-y1>0)) || (color == Red && (y2-y1<0))

valPlyr :: Piece -> Player -> Bool
valPlyr (King plyr) turn = (plyr == turn)
valPlyr (Reg  plyr) turn = (plyr == turn)

-- will implement a counter that cuts game at certain point
{-
winner :: Game -> Maybe Outcome
winner (board, p, count)
    | tie = Just Tie
    | redWins = Just $ Won Red
    | blackWins = Just $ Won Black
    | otherwise = Nothing
    where tie = 
          redWins = all (\square -> (snd square) == (Reg Red)  ||
                                  (snd square) == (King Red) ||
                                  (snd square) == (Empty)) board
          blackWins = all (\square -> (snd square) == (Reg Black)  ||
                                    (snd square) == (King Black) ||
                                    (snd square) == (Empty)) board
-}
winner :: Game -> Maybe Outcome
winner (board,plyr,0) =
    let redPieces = countPieces board Red
        blackPieces = countPieces board Black
        equalPieces = redPieces == blackPieces
        moreRed = (redPieces > blackPieces)
    in  if equalPieces
        then Just Tie
        else if moreRed
             then Just $ Won Red
             else Just $ Won Black 
winner (board,plyr,count)
    | redWins     = Just $ Won Red
    | blackWins   = Just $ Won Black
    | otherwise   = Nothing
    where redWins =   all (\square -> (snd square) == (Reg Red)  ||
                                      (snd square) == (King Red) ||
                                      (snd square) == (Empty)) board
          blackWins = all (\square -> (snd square) == (Reg Black)  ||
                                      (snd square) == (King Black) ||
                                      (snd square) == (Empty)) board

countPieces :: Board -> Player -> Int
countPieces bd plyr = sum [1| sq <- bd, (snd sq) == (Reg plyr) || (snd sq) == (King plyr)]

validMoves :: Game -> [Move]
validMoves (bd, plyer, count) =
  let plSquare = [(loc, pc) | (loc, pc) <- bd, (pc==Reg plyer || pc ==King plyer)]
      mvsForSquare ((r, c), pc) = [((r,c), l) | l <- [(r+1, c+1), (r+1, c-1), (r-1, c-1), (r-1, c+1),
                                   (r+2, c+2), (r+2, c-2), (r-2, c-2), (r-2, c+2)]]
      mvsForPlayer [] = []
      mvsForPlayer (x:xs) = mvsForSquare x++ mvsForPlayer xs
  in [mv | mv <- mvsForPlayer plSquare, validMove (bd, plyer,count) mv]
{-
bestMove :: Game -> (Move, Outcome)
bestMove game =
  case winner game of
    Just g -> (((-1,-1), (-1,-1)), g) -- need to figure out return here to indicate no-move
    Nothing -> bestMoveRec game

bestMoveRec :: Game -> (Move, Outcome)
bestMoveRec (bd, plyr,count) =
  let possMvsAndGames = movesAndGames (bd, plyr,count) -- [(Move, Game)]
      possMvsAndOutcomes = [willWin mvAndGame | mvAndGame <- possMvsAndGames] -- [(Move, Outcome)]
  in bestOutcomeForPlayer plyr possMvsAndOutcomes -- (Move, Outcome)

movesAndGames :: Game -> [(Move, Game)]
movesAndGames gm =
  let mvs = validMoves gm
      gmsForMvs = catMaybes [updateBoard gm mv | mv <- mvs]
  in zip mvs gmsForMvs

willWin :: (Move, Game) -> (Move, Outcome)
willWin (move, (bd, plyr,count)) =
  let res = case winner (bd, plyr,count) of
              Just o -> [(move, o)]
--              Nothing -> map (\mvAndGm -> ++(willWin mvAndGm)) (movesAndGames (bd, plyr))
              Nothing -> foldr (\mAndGm x -> mAndGm ++ x) (map (\mvAndGm -> (willWin mvAndGm)) (movesAndGames (bd, plyr,count))) []
  in bestOutcomeForPlayer plyr res

bestOutcomeForPlayer :: Player -> [(Move, Outcome)] -> (Move, Outcome)
bestOutcomeForPlayer plyr movesAndOuts =
  let wins = [(mv, w) | (mv, w) <- movesAndOuts, w== (Won plyr)]
      ties = [(mv, t) | (mv, t) <- movesAndOuts, t== (Tie)]
      losses = [(mv, l) | (mv, l) <- movesAndOuts, l== (Won (otherPlayer plyr))]
  in if wins /= []
     then head wins
     else if ties /= []
     then head ties
     else head losses
-}
bestMove :: Game -> (Move, Outcome)
bestMove game =
  case winner game of
    Just g -> (((-1,-1), (-1,-1)), g) -- need to figure out return here to indicate no-move
    Nothing -> bestMoveRec game

bestMoveRec :: Game -> (Move, Outcome)
bestMoveRec (bd, plyr,count) =
  let possMvsAndGames = movesAndGames (bd, plyr,count) -- [(Move, Game)]
      possMvsAndOutcomes = [willWin (mv, (mv, gm)) | (mv, gm) <- possMvsAndGames] -- [(Move, Outcome)]
      (initialMv, (winningMv, out)) = bestOutcomeForPlayer plyr possMvsAndOutcomes
  in (initialMv, out) -- (Move, Outcome)

movesAndGames :: Game -> [(Move, Game)]
movesAndGames gm =
  let mvs = validMoves gm
      gmsForMvs = catMaybes [updateBoard gm mv | mv <- mvs]
  in zip mvs gmsForMvs

willWin :: (Move,(Move, Game)) -> (Move, (Move, Outcome))
willWin (initMv, (recMv, (bd, plyr,count))) =
  let res = case winner (bd, plyr,count) of
              Just o -> [(initMv, (recMv, o))]
--              Nothing -> map (\mvAndGm -> ++(willWin mvAndGm)) (movesAndGames (bd, plyr))
              Nothing -> foldr (\mAndO x -> mAndO ++ x) (map (\mvAndGm -> (willWin (initMv, mvAndGm))) (movesAndGames (bd, plyr,count))) []
  in bestOutcomeForPlayer plyr res

bestOutcomeForPlayer :: Player -> [(Move, (Move, Outcome))] -> (Move, (Move, Outcome))
bestOutcomeForPlayer plyr movesAndOuts =
  let wins = [(i, (mv, w)) | (i, (mv, w)) <- movesAndOuts, w== (Won plyr)]
      ties = [(i, (mv, t)) | (i, (mv, t)) <- movesAndOuts, t== (Tie)]
      losses = [(i, (mv, l)) | (i, (mv, l)) <- movesAndOuts, l== (Won (otherPlayer plyr))]
  in if wins /= []
     then head wins
     else if ties /= []
     then head ties
     else head losses




  {-
bestMoves :: Game -> [Move]
bestMoves (bd, plyer) = 
  let allMoves = validMoves (bd, plyer)
  -}

{-
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
-}
