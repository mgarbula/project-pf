module Play where
import Printing

type Move = (String, String)
type Color = Char

fromTo :: String -> Move
fromTo move = (takeWhile (/= '-') move, tail (dropWhile (/= '-') move))

takeFirstElements board move = takeWhile (\b -> first (head b) /= move) board
takeRestElements board move = dropWhile (\b -> first (head b) /= move) board

changePosition :: [Field] -> String -> Color -> [Field]
changePosition [] _ _ = []
changePosition (x:xs) s color
    | second x == head s = (first x, second x, color) : changePosition xs s color
    | otherwise = x : changePosition xs s color

changePositionBoard :: [[Field]] -> String -> Color -> [[Field]]
changePositionBoard board move color = takeFirstElements board number ++ [changePosition (head rest) (snd myMove) color] ++ tail rest
    where myMove = fromTo move
          number = read (tail (snd myMove))
          rest = takeRestElements board number

clearPosition :: [Field] -> String -> [Field]
clearPosition [] _ = []
clearPosition (x:xs) s 
    | second x == head s = (first x, second x, ' ') : clearPosition xs s
    | otherwise = x : clearPosition xs s

clearPositionBoard :: [[Field]] -> String -> [[Field]]
clearPositionBoard board move = takeFirstElements board number ++ [clearPosition (head rest) (fst myMove)] ++ tail rest
    where myMove = fromTo move
          number = read (tail (fst myMove))
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
    