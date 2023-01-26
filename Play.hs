module Play where
import Printing
import GHC.Generics (Generic(from))

type Move = (Position, Position)
type Color = Char
type Position = (Char, Integer)

lengthOfFinal final fields = length (filter (\f -> first f == final) fields) == 8

possibleWin :: [Field] -> Integer -> Integer -> Bool
possibleWin fields one two = lengthOfFinal one fields && lengthOfFinal two fields

playerFields :: [[Field]] -> Color -> [Field]
playerFields board color = filter (\p -> third p == color) (foldl (++) [] board)

noFrom from = \f -> second f /= fst from || first f /= snd from

changePlayerField :: [Field] -> Move -> Color -> [Field]
changePlayerField fields move color = (snd to, fst to, color) : (takeWhile (noFrom from) fields ++ tail (dropWhile (noFrom from) fields))
    where
        from = fst move
        to = snd move

fromTo :: [String] -> [Position]
fromTo [] = []
fromTo (l:ls) = (head firstPosition, read (tail firstPosition) :: Integer) : fromTo ls
    where firstPosition = takeWhile (/= '-') l
          secondPosition = tail (dropWhile (/= '-') l)

takeFirstElements board move = takeWhile (\b -> first (head b) /= move) board
takeRestElements board move = dropWhile (\b -> first (head b) /= move) board

changePosition :: [Field] -> Position -> Color -> [Field]
changePosition [] _ _ = []
changePosition (x:xs) s color
    | second x == fst s = (first x, second x, color) : changePosition xs s color
    | otherwise = x : changePosition xs s color

changePositionBoard :: [[Field]] -> String -> Color -> [[Field]]
changePositionBoard board move color = takeFirstElements board number ++ [changePosition (head rest) to color] ++ tail rest
    where myMove = fromTo (moves move)
          to = last myMove
          number = snd to
          rest = takeRestElements board number

clearPosition :: [Field] -> Position -> [Field]
clearPosition [] _ = []
clearPosition (x:xs) s 
    | second x == fst s = (first x, second x, ' ') : clearPosition xs s
    | otherwise = x : clearPosition xs s

clearPositionBoard :: [[Field]] -> String -> [[Field]]
clearPositionBoard board move = takeFirstElements board number ++ [clearPosition (head rest) from] ++ tail rest
    where myMove = fromTo (moves move)
          from = head myMove
          number = snd from
          rest = takeRestElements board number

makeMove :: [[Field]] -> String -> Color -> [[Field]]
makeMove board move = changePositionBoard (clearPositionBoard board move) move

column what = snd (head (dropWhile (\v -> fst v /= fst what) valueOfLetters))
row = snd

differenceBetweenPosition :: Position -> Position -> (Integer, Integer) -- (kolumna, wiersz)
differenceBetweenPosition from to = (column to - column from, row to - row from)

moveOne :: Position -> Position -> Field -> Bool
moveOne from to fieldTo = (x == 0 ||  y == 0) && (abs x == 1 || abs y == 1) && column from + x == column to && snd from + y == snd to && third fieldTo == ' '
    where diff = differenceBetweenPosition from to
          x = fst diff
          y = snd diff
          
valOfLet letter = snd (head (dropWhile (\l -> fst l /= letter) valueOfLetters))

betweenInColumn :: [[Field]] -> Position -> Field
betweenInColumn board from = head (dropWhile (\f -> second f /= fst from) (head (dropWhile (\r -> first (head r) /= snd from + 1) board)))

betweenInRow :: [[Field]] -> Position -> Field
betweenInRow board from = head (tail (dropWhile (\f -> second f /= fst from) (head (dropWhile (\r -> first(head r) /= snd from) board))))

fieldBetween :: [[Field]] -> Position -> Position -> Field
fieldBetween board from to 
    | x == 2 && y == 0 = betweenInRow board from
    | x == -2 && y == 0 = betweenInRow board to
    | x == 0 && y == 2 = betweenInColumn board from
    | x == 0 && y == -2 = betweenInColumn board to
    | otherwise = (9, 'Z', ' ')
    where diff = differenceBetweenPosition from to
          x = fst diff
          y = snd diff

jump :: [[Field]] -> [Position] -> Bool
jump board [] = True
jump board [_] = True
jump board (m:ms) = third (fieldBetween board m to) /= ' ' && jump board ms
    where to = head ms

getField field board = head (dropWhile (\f -> second f /= fst field) (head (dropWhile (\t -> first (head t) /= snd field) board)))

fieldCorrect field = elem (fst field) topLetters && elem (snd field) numbers

howMany :: (Eq a) => a -> [a] -> Int
howMany _ [] = 0
howMany q (x:xs) 
    | q == x = 1 + howMany q xs
    | otherwise = howMany q xs

correctFormatOfDash :: String -> Bool
correctFormatOfDash [] = False
correctFormatOfDash [_] = False
correctFormatOfDash [x,y] = x /= '-' && y /= '-' 
correctFormatOfDash (x:y:xs) = x /= '-' && y /= '-' && head xs == '-' && correctFormatOfDash (tail xs)

correctFormat :: String -> Bool
correctFormat move = correctFormatOfDash move && length move == 3 * howMany '-' move + 2

moves :: String -> [String]
moves [] = []
moves [_] = []
moves [x, y] = [[x, y]]
moves (x:y:xs) = [x, y] : moves (tail xs)

correctSigns :: String -> Bool
correctSigns = foldl (\acc m -> acc && elem m correctSigns) True
    where correctSigns = '-' : topLetters ++ ['1'..'9']

possibilityOfMove :: [[Field]] -> String -> Color -> Bool
possibilityOfMove board move color = correctFormat move && correctSigns move && (length myMoves == 2 && moveOne (head myMove) (head (tail myMove)) fieldTo || jump board myMove)
    where myMoves = moves move
          myMove = fromTo myMoves 
          to = head (tail myMove)
          fieldTo = getField to board