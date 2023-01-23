module Play where
import Printing

type Move = (Position, Position)
type Color = Char
type Position = (Char, Integer)

lengthOfFinal final fields = length (filter (\f -> first f == final) fields) == 8

possibleWin :: [Field] -> Integer -> Integer -> Bool
possibleWin fields one two = lengthOfFinal one fields && lengthOfFinal two fields

playerFields :: [[Field]] -> Color -> [Field]
playerFields board color = filter (\p -> third p == color) (foldl (\acc t -> acc ++ t) [] board)

noFrom from = \f -> second f /= fst from || first f /= snd from

changePlayerField :: [Field] -> Move -> Color -> [Field]
changePlayerField fields move color = (snd to, fst to, color) : (takeWhile (noFrom from) fields ++ tail (dropWhile (noFrom from) fields))
    where
        from = fst move
        to = snd move

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

moveForward :: Position -> Position -> Field -> Bool
moveForward from to fieldTo = fst from == fst to && snd from == snd to + 1 && third fieldTo == ' '

valOfLet letter = snd (head (dropWhile (\l -> fst l /= letter) valueOfLetters))

jump :: Position -> Position -> Field -> Bool
jump from to fieldTo = (abs (snd from - snd to) == 2 && abs (valOfLet (fst from) - valOfLet (fst to)) == 1) && third fieldTo == ' ' || (abs (snd from - snd to) == 1 && abs (valOfLet (fst from) - valOfLet (fst to)) == 2) && third fieldTo == ' '

getField field board = head (dropWhile (\f -> second f /= fst field) (head (dropWhile (\t -> first (head t) /= snd field) board)))

fieldCorrect field = elem (fst field) topLetters && elem (snd field) numbers

howMany :: (Eq a) => a -> [a] -> Int
howMany _ [] = 0
howMany q (x:xs) 
    | q == x = 1 + howMany q xs
    | otherwise = howMany q xs

correctFormat :: String -> Bool
correctFormat move = length move == 5 && head (tail (tail move)) == '-' && howMany '-' move == 1

possibilityOfMove :: [[Field]] -> String -> Color -> Bool
possibilityOfMove board move color = correctFormat move && fieldCorrect from && fieldCorrect to && third fieldFrom == color && (moveForward from to fieldTo || jump from to fieldTo)
    where myMove = fromTo move 
          from = fst myMove
          to = snd myMove
          fieldFrom = getField from board
          fieldTo = getField to board