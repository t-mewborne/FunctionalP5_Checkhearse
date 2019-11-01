module checkhears where

data Player = Black | Red
data Piece = Reg Player | King Player | Empty


type Loc = (Int,Int)
type Square = (Loc,Piece)
type Board = [[Piece]]
--type Board = [Square]
type Game = (Board,Player)

type Board = [Square]
type Move ((Char,Int),(Char,Int)) --((Start),(End),Turn)

showBoard :: Game ->  [String] --One string per row
showBoard = undefined

updateBoard :: Game -> Move -> Maybe Board --is the move valid? Should we make this a return a Maybe Game?
updateBoard = undefined

validMove :: Game -> Move -> Bool
validMove = undefined

readLoc :: String -> Loc --this should return a maybe loc in the future
readLoc = undefined
