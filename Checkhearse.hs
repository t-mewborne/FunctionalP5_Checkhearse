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
type Move = (Loc,Loc) --(Start,Destination)

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

-- take in current game and move, return Maybe Game
-- calculates if jump or scoot, then makes that move if valid
updateBoard :: Game -> Move -> Maybe Game
updateBoard game mv =
  if not (validMove game mv) then Nothing
  else if abs (r2-r1)==1
       then makeScoot game mv 
       else makeJump game mv
  where ((r1,_), (r2,_)) = mv

--validMove checks: if start & dest. are on the board,
--                     count > 0,
--                     start is player's color,
--                     piece is moving correct direction
--                     & jumping other player (if jump)

validMove :: Game -> Move -> Bool
validMove (bd,plyr,count) (startLoc,endLoc) =
    let startPc = lookup startLoc bd
        endPc = lookup endLoc bd
    in  case (startPc,endPc) of
            (Just start,Just Empty) -> count > 0 && valPlyr start plyr 
                                                 && rightDir (startLoc,start) (endLoc,Empty) 
            _ -> False

-- find victim, if is one
jumpedPiece :: Move -> Board -> Maybe Square
jumpedPiece ((r1,c1), (r2,c2)) bd =
   do let cv = if (c2-c1>0) -- victim col
               then c1+1
               else c1-1
      let rv = if (r2-r1>0) -- victim row
               then r1+1
               else r1-1
      victim <- lookup (rv, cv) bd
      return ((rv, cv), victim)

assertTrue :: Bool -> Maybe ()
assertTrue True = Just ()
assertTrue False = Nothing

makeJump :: Game -> Move -> Maybe Game
makeJump (bd, plyr, count) (start, end) =
    do (vicLoc, victim) <- jumpedPiece (start, end) bd
       activePiece <- lookup start bd
       assertTrue (victim `notElem` [Reg plyr, King plyr, Empty])
       let removeStart = removePiece bd start
           removeVictim = removePiece removeStart end
           newBoard = setPiece (removeVictim, plyr, count) end activePiece
       return (newBoard, (otherPlayer plyr), count-1)

otherPlayer :: Player -> Player
otherPlayer Red = Black
otherPlayer Black = Red

makeScoot :: Game -> Move -> Maybe Game
makeScoot (bd, plyr,count) (start, end) =
  do activePiece <- lookup start bd
     let removeStart = removePiece bd start
         newBoard = setPiece (removeStart, plyr, count) end activePiece
     return (newBoard, (otherPlayer plyr), count-1)

-- Places piece in loc & metamorphosizes to king if earned
setPiece :: Game -> Loc -> Piece -> Board
setPiece (bd, plyr, count) (r,c) replacement = 
  let newPiece = case (r, replacement) of
                   (1, (Reg Red))   -> King Red
                   (8, (Reg Black)) -> King Black
                   _                -> replacement
  in [ if x==(r,c) then ((r,c),newPiece) else (x,piece) | (x,piece) <- bd]
 
-- Doesn't return Maybe Board, bc we checked for location already
removePiece :: Board -> Loc -> Board
removePiece bd loc = [ if x==loc then (loc, Empty) else (x,y) | (x,y) <- bd]

--Make sure you are moving in the right direction based on piece and its color
rightDir :: Square -> Square -> Bool
rightDir ((y1,x1),King _) ((y2,x2),Empty) = True
rightDir ((y1,x1),Reg color) ((y2,x2),Empty) = (color == Black && (y2-y1>0)) || (color == Red && (y2-y1<0))

--Make sure right player move for turn
valPlyr :: Piece -> Player -> Bool
valPlyr (King plyr) turn = (plyr == turn)
valPlyr (Reg  plyr) turn = (plyr == turn)

-- winner tells if game is in winning state
-- first case: counter is at 0
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

-- 2nd case: counter>0, if no winner, returns Nothing
winner (board,plyr,count)
    | (win Red)   = Just $ Won Red
    | (win Black) = Just $ Won Black
    | otherwise   = Nothing
    where win p = all (\square -> (snd square) == (Reg p)  ||
                                  (snd square) == (King p) ||
                                  (snd square) == (Empty)) board

-- count a player's pieces: helps determine winner
countPieces :: Board -> Player -> Int
countPieces bd plyr = sum [1| sq <- bd, (snd sq) == (Reg plyr) || (snd sq) == (King plyr)]

-- all validMoves for current player 
validMoves :: Game -> [Move]
validMoves (bd, plyer, count) =
  let plSquare = [(loc, pc) | (loc, pc) <- bd, (pc==Reg plyer || pc ==King plyer)]
      mvsForSquare ((r, c), pc) = [((r,c), l) | l <- [(r+1, c+1), (r+1, c-1), (r-1, c-1), (r-1, c+1),
                                   (r+2, c+2), (r+2, c-2), (r-2, c-2), (r-2, c+2)]]
      mvsForPlayer [] = []
      mvsForPlayer (x:xs) = mvsForSquare x ++ mvsForPlayer xs
  in [mv | mv <- mvsForPlayer plSquare, validMove (bd, plyer,count) mv]

-- returns single bestMove & outcome 
bestMove :: Game -> (Move, Outcome)
bestMove (bd, plyr, count) =
  let possMvsAndGames = movesAndGames (bd, plyr,count) -- [(Move, Game)]
      possMvsAndOutcomes = [willWin (mv, (mv, gm)) | (mv, gm) <- possMvsAndGames] -- [(Move, Outcome)]
      (initialMv, (winningMv, out)) = bestOutcomeForPlayer plyr possMvsAndOutcomes
  in case winner (bd, plyr, count) of
      Just g -> (((-1,-1), (-1,-1)), g) -- NO MOVE: BOARD ALREADY WON
      Nothing -> (initialMv, out)

-- take current gamestate & return possible next gamestates
movesAndGames :: Game -> [(Move, Game)]
movesAndGames gm =
  let mvs = validMoves gm
      gmsForMvs = catMaybes [updateBoard gm mv | mv <- mvs]
  in zip mvs gmsForMvs

-- recursively call bestOutcomeForPlayer & itself to return outcome
-- returns (initialMove, (finalMove, Outcome))
willWin :: (Move,(Move, Game)) -> (Move, (Move, Outcome))
willWin (initMv, (recMv, (bd, plyr,count))) =
  let res = case winner (bd, plyr,count) of
              Just o -> [(initMv, (recMv, o))]
              Nothing -> foldr (\mAndO x -> mAndO ++ x) (map (\mvAndGm -> (willWin (initMv, mvAndGm))) (movesAndGames (bd, plyr,count))) []
  in bestOutcomeForPlayer plyr res

-- chooses recursing move, based on player
-- takes in list of (initialMove, (recursingMove, Outcome)) 
-- returns elem from list with best outcome for player
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

--Gives the rank of a board. If rank > 0, black is winning. Otherwise, red is winning. If it is
--equal to 0, it's tied.
--If rank returns 100, black wins. If rank returns -100, red wins.
rank :: Game -> Int
rank (bd,plyr,c) = 
    let redCt = sum [1| sq <- bd, (snd sq) == (Reg Red)] + sum [2| sq <- bd, (snd sq) == (King Red)]
        blkCt = sum [1| sq <- bd, (snd sq) == (Reg Black)] + sum [2| sq <- bd, (snd sq) == (King Black)]
    in case winner (bd,plyr,c) of
            Just (Won Black) -> 100
            Just (Won Red)   -> -100
            _ -> (blkCt - redCt)

-- does recursive call to return bestMove & its rank, based on depth given
depthSearch :: Game -> Int -> (Move,Int)
depthSearch (bd,plyr,ct) cut = 
    let possMvsAndGames = movesAndGames (bd,plyr,ct)
        possMvsAndRanks = [bestBoard (mv, (mv, gm)) cut | (mv, gm) <- possMvsAndGames]
        (i, (_,r)) = bestRankForPlayer plyr possMvsAndRanks
    in  if (cut >= ct)
        then error $ "(depthSearch) Cut (" ++ (show cut) ++ ") cannot be greater than turns remaining (" ++ (show ct) ++ ")."
        else (i,r)

-- chooses from list of (initialMove, (recurseMove, Rank)) 
-- based on which has best rank for player at time
bestRankForPlayer :: Player -> [(Move,(Move,Int))] -> (Move,(Move,Int))
bestRankForPlayer plyr movesNRks =
    let ranks = [r|(i,(m,r)) <- movesNRks]
        bestRank = case plyr of
                       Red -> minimum ranks
                       Black -> maximum ranks
    in head [(i,(m,r)) | (i,(m,r)) <- movesNRks, r == bestRank]

-- recursively calls itself & bestRankForPlayer to return 
-- (initialMove, (finalMove, Rank))
bestBoard :: (Move,(Move, Game)) -> Int -> (Move, (Move, Int))
bestBoard (i, (m, (bd, plyr,count))) cut =
    let res = case cut of 
                   0 -> [(i,(m,rank (bd, plyr,count)))]
                   other -> foldr (\mAndR x -> mAndR ++ x) (map (\mvAndGm -> (bestBoard (i, mvAndGm) (cut-1))) (movesAndGames (bd,plyr,count))) []
    in  bestRankForPlayer plyr res
