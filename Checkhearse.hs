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
updateBoard game ((r1, c1), (r2, c2)) =
  if not (validMove game ((r1, c1), (r2, c2))) then Nothing
  else if abs (r2-r1)==1
    then makeScoot game ((r1, c1), (r2, c2))  
  else makeJump game ((r1, c1), (r2, c2))  

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
                                                 && checkForVictim (bd, plyr,count) (startLoc, endLoc)
            _ -> False

findVictim :: Move -> Square
findVictim ((r1, c1), (r2, c2)) =
  -- find victim

-- if move is scoot, then returns true, because there's no victim to check
-- if move is jump, checks identify of victim (other player) & return true if correct
-- return false if jump over self or empty square
checkForVictim :: Game -> Move -> Bool
checkForVictim (bd, plyr,count) ((r1,c1), (r2,c2)) =
   let cv = if (c2-c1>0) -- victim col
             then c1+1
             else c1-1
       rv = if (r2-r1>0) -- victim row
             then r1+1
             else r1-1
       victim = 
            case lookup (rv, cv) bd of
                    Nothing -> Empty
                    Just v -> v
   in if abs (r2-r1)==1 then True -- if it's a scoot, it returns true
      else if (victim `elem` [Reg plyr, King plyr, Empty]) -- if jumping over own piece or empty square, then false
      then False
      else True 


jumpedPiece (r1,c1) (r2,c2) 

	where inBetween n1 n2 
			| n2-n1 > 0 = 

assertTrue :: Bool -> Maybe ()
assertTrue True = Just ()
assertTrue False = Nothing

makeJump :: Game -> Move -> Maybe Game
makeJump (bd, plyr, count) ((r1, c1), (r2, c2)) =
    do let cv = if (c2-c1>0) -- to the right
             then c1+1
             else c1-1 -- to the left
       let rv = if (r2-r1>0) -- to the bottom
             then r1+1
             else r1-1 -- to the top
       victim <- lookup (rv, cv) bd
       activePiece <- lookup (r1, c1) bd
       assertTrue (victim `elem` [Reg plyr, King plyr, Empty])
       refreshBoard = changeBoard _________
       --setPiece (setPiece remStart (end,Empty) activePiece, nextTurn, count-1)
       (newBoard, (otherPlayer plyr), count-1)
--       Just $ doJump (r1, c1) (r2, c2) ((rv,cv),victim) (bd,plyr,count)


doJump :: Loc -> Loc -> Square -> Game -> Game
doJump start end victim (bd,plyr,count) =
    let activePiece =
            case lookup start bd of
                Just piece -> piece
                _ -> error "(doJump) Invalid start piece"
        remStart = setPiece bd (start, activePiece) Empty
        remVictim = setPiece remStart victim Empty
        nextTurn = otherPlayer plyr
    in  (setPiece remVictim (end, Empty) activePiece, nextTurn, count-1)

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

remPiece :: Board -> Loc -> Maybe Board
setPiece bd loc replacement 
	| lookup loc bd == Nothing = Nothing 
	| otherwise                = [ if x==loc then (loc,Empty) else (x,y) | (x,y) <- bd]


addPiece :: Board -> Piece -> Maybe Board

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

depthSearch :: Game -> Int -> (Move,Int)
depthSearch (bd,plyr,ct) cut = 
    let possMvsAndGames = movesAndGames (bd,plyr,ct)
        possMvsAndRanks = [bestBoard (mv, (mv, gm)) cut | (mv, gm) <- possMvsAndGames]
        (i, (_,r)) = bestRankForPlayer plyr possMvsAndRanks
    in  if (cut >= ct)
        then error $ "(depthSearch) Cut (" ++ (show cut) ++ ") cannot be greater than turns remaining (" ++ (show ct) ++ ")."
        else (i,r)

bestRankForPlayer :: Player -> [(Move,(Move,Int))] -> (Move,(Move,Int))
bestRankForPlayer plyr movesNRks =
    let ranks = [r|(i,(m,r)) <- movesNRks]
        bestRank = case plyr of
                       Red -> minimum ranks
                       Black -> maximum ranks
    in head [(i,(m,r)) | (i,(m,r)) <- movesNRks, r == bestRank]

bestBoard :: (Move,(Move, Game)) -> Int -> (Move, (Move, Int))
bestBoard (i, (m, (bd, plyr,count))) cut =
    let res = case cut of 
                   0 -> [(i,(m,rank (bd, plyr,count)))]
                   other -> foldr (\mAndR x -> mAndR ++ x) (map (\mvAndGm -> (bestBoard (i, mvAndGm) (cut-1))) (movesAndGames (bd,plyr,count))) []
    in  bestRankForPlayer plyr res
