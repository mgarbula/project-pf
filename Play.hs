module Play where
import Printing

type Move = (Position, Position)
type Color = Char
type Position = (Char, Integer)

fromTo :: String -> Move
fromTo move = ((head firstPosition, read (tail firstPosition) :: Integer), (head secondPosition, read (tail secondPosition) :: Integer))
    where firstPosition = takeWhile (/= '-') move
          secondPosition = tail (dropWhile (/= '-') move)

takeFirstElements board move = takeWhile (\b -> first (head b) /= move) board
takeRestElements board move = dropWhile (\b -> first (head b) /= move) board

changePosition :: [Field] -> Position -> Color -> [Field]
changePosition [] _ _ = []
changePosition (x:xs) s color
    | second x == fst s = (first x, second x, color) : changePosition xs s color
    | otherwise = x : changePosition xs s color

changePositionBoard :: [[Field]] -> String -> Color -> [[Field]]
changePositionBoard board move color = takeFirstElements board number ++ [changePosition (head rest) (snd myMove) color] ++ tail rest
    where myMove = fromTo move
          number = snd (snd myMove)
          rest = takeRestElements board number

clearPosition :: [Field] -> Position -> [Field]
clearPosition [] _ = []
clearPosition (x:xs) s 
    | second x == fst s = (first x, second x, ' ') : clearPosition xs s
    | otherwise = x : clearPosition xs s

clearPositionBoard :: [[Field]] -> String -> [[Field]]
clearPositionBoard board move = takeFirstElements board number ++ [clearPosition (head rest) (fst myMove)] ++ tail rest
    where myMove = fromTo move
          number = snd (fst myMove)
          rest = takeRestElements board number

makeMove :: [[Field]] -> String -> Color -> [[Field]]
makeMove board move color = changePositionBoard (clearPositionBoard board move) move color


go :: [[Field]] -> String -> IO ()
go board color = do
    putStrLn "Twoj ruch"
    move <- getLine
    let newBoard = makeMove board move (myColor color)
    putStrLn (printAll newBoard)
    go newBoard color
    