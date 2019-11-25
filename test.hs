import Checkhearse


{- Ongoing Errors
*Main> validMove startBlack s7
True
*Main> validMove startBlack s6
True
*Main> validMove startBlack s5
True
*Main> validMove startBlack s4
True
*Main> validMove startBlack s3
True
*Main> validMove startBlack s2
False
*Main> validMove startBlack s1
False

*Main> validMove startBlack s1
True
*Main> validMove startBlack s2
True
*Main> validMove startBlack s3
True
*Main> validMove startBlack s4
True
*Main> validMove startBlack s5
True
*Main> validMove startBlack s6
True
*Main> validMove startBlack s7
True
*Main> validMove startBlack s8



-}
{-
*Main> rightDir (3,2) (Just (King Black)) (4,3) (Just Empty) Black
True
*Main> rightDir (3,2) (Just (King Black)) (4,1) (Just Empty) Black
True
*Main> rightDir (3,2) (Just (Reg Black)) (3,2) (Just Empty) Black
False
*Main> rightDir (3,2) (Just (Reg Black)) (4,2) (Just Empty) Black
True
*Main> rightDir (3,2) (Just (Reg Black)) (5,2) (Just Empty) Black
True
*Main> rightDir (3,2) (Just (Reg Black)) (0,2) (Just Empty) Black
False
*Main> rightDir (3,2) (Just (Reg Black)) (0,0) (Just Empty) Black
False


-}

boardFromGame :: Game -> Board
boardFromGame (x,y) = x

{-
boardBlack :: Board -> IO ()
boardBlack board = putStrLn $ showBoard (board, Black)

moveBoard :: Move -> IO ()
moveBoard mov = putStrLn $ showBoard (updateBoard startBlack mov)

gameBoard :: Game -> IO ()
gameBoard game = putStrLn $ showBoard game
-}
-- putStrLn $ showBoard buildGame
-- Start Position, Black Turn
startBlack :: Game
startBlack = ([((1,2),Reg Black),((1,4),Reg Black),((1,6),Reg Black),((1,8),Reg Black),((2,1),Reg Black),((2,3),Reg Black),((2,5),Reg Black),((2,7),Reg Black),((3,2),Reg Black),((3,4),Reg Black),((3,6),Reg Black),((3,8),Reg Black),((4,1),Empty),((4,3),Empty),((4,5),Empty),((4,7),Empty),((5,2),Empty),((5,4),Empty),((5,6),Empty),((5,8),Empty),((6,1),Reg Red),((6,3),Reg Red),((6,5),Reg Red),((6,7),Reg Red),((7,2),Reg Red),((7,4),Reg Red),((7,6),Reg Red),((7,8),Reg Red),((8,1),Reg Red),((8,3),Reg Red),((8,5),Reg Red),((8,7),Reg Red)],Black)

s1 :: Move
s1 = ((3,2),(4,1))

s2 :: Move
s2 = ((3,2),(5,2))

s3 :: Move
s3 = ((3,2),(4,3))

s4 :: Move
s4 = ((3,2),(5,4))

s5 :: Move
s5 = ((3,2),(4,5))

s6 :: Move
s6 = ((3,2),(5,6))

s7 :: Move
s7 = ((3,2),(4,7))

-- Start Position, Red Turn
startRed :: Game
startRed = ([((1,2),Reg Black),((1,4),Reg Black),((1,6),Reg Black),((1,8),Reg Black),((2,1),Reg Black),((2,3),Reg Black),((2,5),Reg Black),((2,7),Reg Black),((3,2),Reg Black),((3,4),Reg Black),((3,6),Reg Black),((3,8),Reg Black),((4,1),Empty),((4,3),Empty),((4,5),Empty),((4,7),Empty),((5,2),Empty),((5,4),Empty),((5,6),Empty),((5,8),Empty),((6,1),Reg Red),((6,3),Reg Red),((6,5),Reg Red),((6,7),Reg Red),((7,2),Reg Red),((7,4),Reg Red),((7,6),Reg Red),((7,8),Reg Red),((8,1),Reg Red),((8,3),Reg Red),((8,5),Reg Red),((8,7),Reg Red)],Black)

-- Start Position with Piece in Invalid Location, Red Turn
-- Still compiles and the invalid piece is not shown.
specialBlack :: Game
specialBlack = ([((1,1),Reg Black), ((1,2),Reg Black),((1,4),Reg Black),((1,6),Reg Black),((1,8),Reg Black),((2,1),Reg Black),((2,3),Reg Black),((2,5),Reg Black),((2,7),Reg Black),((3,2),Reg Black),((3,4),Reg Black),((3,6),Reg Black),((3,8),Reg Black),((4,1),Empty),((4,3),Empty),((4,5),Empty),((4,7),Empty),((5,2),Empty),((5,4),Empty),((5,6),Empty),((5,8),Empty),((6,1),Reg Red),((6,3),Reg Red),((6,5),Reg Red),((6,7),Reg Red),((7,2),Reg Red),((7,4),Reg Red),((7,6),Reg Red),((7,8),Reg Red),((8,1),Reg Red),((8,3),Reg Red),((8,5),Reg Red),((8,7),Reg Red)],Black)


-- Empty Board, Black Turn
emptyBlack :: Game
emptyBlack = ([],Black)

-- One Piece, Black Turn
oneBlack :: Game
oneBlack = ([((1,2),Reg Black)], Red)
