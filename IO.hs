module IO where
import Checkhearse
import System.Directory
import Data.Char
import System.Environment
import System.Console.GetOpt
import Text.Read
import Data.List.Split


{-
INSTRUCTIONS FOR USAGE:

IF FIRST TIME, PUT THIS IN THE TERMINAL:
> make setup

ELSE
> make

TO RUN (use appropiate flags. add -h at end for help)
> ./checkhearse 

IN TERMINAL:
> ghci
> :load Main.hs
> main

ALSO
runhaskell IO.hs

-}

data Flag = Help | Winner | Current | All | Count String | Move String | Verb | Interact deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print out a help message and exitt the program."
          , Option ['c'] ["current"] (NoArg Current) "Print out the current board from the file."
          , Option ['w'] ["winner"] (NoArg Winner) "Print out the best move using an exhaustive search (no cut-off depth)."
          , Option ['d'] ["depth"] (ReqArg Count "<k>") "Use num as a cut-off depth instead of default."
          , Option ['m'] ["move"] (ReqArg Move "<m>") "Should print out the restulting game."
          , Option ['v'] ["verbose"] (NoArg Verb) "Output a move and the description of the move."
          , Option ['i'] ["interactive"] (NoArg Interact) "Play a game against the computer."
          ]

main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    putStrLn $ show (flags, inputs, errors)
    if Help `elem` flags || not (null errors) || length inputs > 1
    then do mapM putStr errors
            putStrLn $ usageInfo "Usage: ./checkhearse file [options]" options
    else do let fileName = head inputs
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
                game = loadGame rows
                moveShort = (moveTerm (head $ tail inputs))
                
            --if Winner `elem` flags
            --then putStrLn $ "You are fun."
            if Current `elem` flags
            then putStrLn $ showBoard game
            else do let moveBoard = updateBoard game moveShort
                    case moveBoard of
                        Just game -> putStrLn $ showBoard $ game
                        _ -> putStrLn $ "Invalid game"
                 
            --if Move `elem` flags
            --then putStrLn $ showBoard $ updateBoard game move
            -- ./checkhearse game.txt 1,2 3,4 -> convert string to ints -> convert ints to tuple ->
            -- convert tuple to move
            -- splitAt "," 1 2 -> read "1" = 1 -> 1 = a, 2 = b (a, b)
            --else putStrLn $ "Hi"
            --if Winner `elem` flags
            --then showBestMove ((1,2),(2,1))
            --else 

-- ./checkhearse game.txt 1,2 3,4 -> "1,2" -> "1" "2" -> 1 2
-- ./checkhearse game.txt 1,2,3,4


getMove :: [Flag] -> String
getMove (Move s:_) = s :: String
getMove (_:flags) = getMove flags
getName [] = error "Please enter a move."


getCount :: [Flag] -> Maybe Int
getCount (Count s:_) = readMaybe s
getCount (_:flags) = getCount flags
getCount [] =  Nothing

moveTerm :: String -> Move
moveTerm first =
  let a = [read x::Int | x <- splitOn "," first]
      b = (head a, head $ tail a)
      c = (head $ tail $ tail a, last a)
  in  (b,c)

--Ask the user a question
prompt :: String -> IO String
prompt message =
    do putStr message
       getLine

--Load the game from the file
loadGame :: [String] -> Game
loadGame (count:turn:board) =
    let k = read count
        plyr = if (turn == "1") then Black else Red 
        aux :: [String] -> Loc -> Board
        aux _ (9,_) = []
        aux (row:rows) (r,c) = (loadRow row (r,c)) ++ (aux rows (r+1,c))
    in (aux board (1,1), plyr,k)
loadGame lst = error "(loadGame) error loading game"
    
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
writeGame (board,turn,count) fileName = 
    let plyr = if (turn == Black) then "1\n" else "2\n"
        k = (show count) ++ "\n"
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
    in writeFile fileName (k ++ plyr ++ (aux board (1,1)))

--Search the board to determine if there is a piece at the given location
searchBoard :: Board -> Loc -> Piece
searchBoard board loc = 
    case lookup loc board of
         Just piece -> piece
         Nothing -> Empty

--This function prints any move passed to it with the text "Best Move: "

{-
aux :: Move -> [String]
aux ((r1,c1),(r2,c2)) = "Move the piece at (" ++ show r1 ++ ", " ++ show c1 ++
    ") to (" ++ show r2 ++ ", " ++ show c2 ++ ").\n" -- ++ aux move

showBestMove :: Move -> IO () 

showBestMove move = putStr $ "Best Move: " ++ (aux move)
-}

showBestMove move =
    let aux :: Move -> String
        aux ((r1,c1),(r2,c2)) = 
            "Move the piece at (" ++ show r1 ++ ", " ++ show c1 ++") to (" ++ 
            show r2 ++ ", " ++ show c2 ++ ").\n"
    in putStr $ "Best Move: " ++ (aux move)
            
showBoard :: Game -> String
showBoard (bd,plyr,count) = 
    let seperator =   "    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
        columnLabels ="\n       1     2     3     4     5     6     7     8\n"
        top = columnLabels ++ seperator
        k = "  Turns remaining: " ++ show count ++ "\n"
        showRows::Board -> Int -> String
        showRows board row 
            | row >= 9 = []
            | otherwise  = (showRow board row) ++ seperator ++ (showRows board (row + 1)) 
        showTurn = 
            case plyr of
            Black -> "\n\x1b[30m  Player 1's turn (Black Pieces)\x1b[0m\n"
            Red -> "\n\x1b[31m  Player 2's turn (Red Pieces)\x1b[0m\n" 
    in top ++ (showRows bd 1) ++ showTurn ++ k

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



{-
print format for showBoard:

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

{-
file format of buildGame:
75         |  COUNTER
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

