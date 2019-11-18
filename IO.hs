module IO where
import Checkhearse
import System.Directory


main :: IO ()
main = do
    fileName <- prompt "Please enter the file name: " -- Should we keep the file name the same every time
    {-exists <- doesFileExist fileName
     -let game = 
     -  if (exists)
     -  then  do board <- readFile fileName
     -        let rows = lines board
     -        loadGame rows
     -  else buildGame
     -}
    board <- readFile fileName
    let rows = lines board
    let game = loadGame rows
    putStrLn $ showBoard game

--Ask the user a question
prompt :: String -> IO String
prompt message =
    do putStr message
       getLine

--Load the game from the file
loadGame :: [String] -> Game
loadGame (turn:board) =
    let plyr = if (turn == "1") then Black else Red 
        aux :: [String] -> Loc -> Board
        aux _ (9,_) = []
        aux (row:rows) (r,c) = (loadRow row (r,c)) ++ (aux rows (r+1,c))
    in (aux board (1,1), plyr)
    
--Load an individual row from the file
loadRow :: String -> Loc -> Board
loadRow [] _ = []
loadRow (space:spaces) (r,c) =
    (case space of
        '0' -> ((r,c), Empty)
        '1' -> ((r,c), Reg Black)
        '2' -> ((r,c), Reg Red)
        '3' -> ((r,c), King Black)
        '4' -> ((r,c), King Red)):(loadRow spaces (r,c+1))

--Write a game to a file
writeGame :: Game-> String -> IO ()
writeGame (board,turn) fileName = 
    let plyr = if (turn == Black) then "1\n" else "2\n"
        aux :: Board -> Loc -> String
        aux _ (r,9) = '\n':(aux board (r+1,1))
        aux _ (9,_) = []
        aux board (r,c) =
            (case searchBoard board (r,c) of
                Empty      -> '0'
                Reg Black  -> '1'
                Reg Red    -> '2'
                King Black -> '3'
                King Red   -> '4'):(aux board (r,c+1))
    in writeFile fileName (plyr ++ (aux board (1,1)))

--Search the board to determine if there is a piece at the given location
searchBoard :: Board -> Loc -> Piece
searchBoard board loc = 
    case lookup loc board of
         Just piece -> piece
         Nothing -> Empty

--This function prints any move passed to it with the text "Best Move: "
showBestMove :: Move -> IO () 
showBestMove move =
    let aux :: Move -> String
        aux ((r1,c1),(r2,c2)) "Move the piece at (" ++ show r1 ++ ", " ++ show c1 ++
            ") to (" ++ show r2 ++ ", " ++ show c2 ++ ").\n" ++ aux moves
    in putStr $ "Best Move: " ++ (aux move)
            

{-
file format of buildGame:

1          |  PLAYER TURN
01010101   |  ROW 1 (Black Pieces)
10101010   |
01010101   |
00000000   |  ROW 4 (Empty)
00000000   |
20202020   |  ROW 5 (Red Pieces)
02020202   |
20202020   |

*zeros can represent empty spaces or invalid spaces
*the first line represents who's turn it is
*3 and 4 represent king pieces (3=king black; 4=king red)
-}
