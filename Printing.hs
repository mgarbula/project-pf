module Printing where

topLetters = ['A'..'H']
numbers = [1..8]
lengthOfBoard = 8
printSpaceForJumper = "|     "
printLine = "|-----"
printEquals = "|====="
endLine = "||\n"

oponentColor :: String -> Char
oponentColor c
    | c == "B" = 'C'
    | otherwise = 'B'

myColor :: String -> Char
myColor c
    | c == "B" = 'B'
    | otherwise = 'C'

fillBoard :: [[(Integer, Char, Char)]] -> String -> [[(Integer, Char, Char)]]
fillBoard [] _ = []
fillBoard (x:xs) c
    | first (head x) == 1 || first (head x) == 2 = (map (\(q, w, _) -> (q, w, oponentColor c)) x) : fillBoard xs c
    | first (head x) == 7 || first (head x) == 8 = (map (\(q, w, _) -> (q, w, myColor c)) x) : fillBoard xs c
    | otherwise = x : fillBoard xs c

makeBoard :: [Integer] -> [Char] -> String -> [[(Integer, Char, Char)]]
makeBoard n l c = fillBoard (makeEmptyBoard n l c) c

makeEmptyBoard :: [Integer] -> [Char] -> String -> [[(Integer, Char, Char)]]
makeEmptyBoard [] _ _ = []
makeEmptyBoard (n:ns) l c = (map (\x -> (n, x, ' ')) l) : makeEmptyBoard ns l c

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

printLetter :: Char -> [Char]
printLetter x = "| " ++ show x ++ " "

printMultipleEquals :: Int -> [Char]
printMultipleEquals 0 = "||"
printMultipleEquals x = printEquals ++ printMultipleEquals (x - 1)

printTop :: [Char] -> [Char]    
printTop [] = endLine ++ "=|" ++ printMultipleEquals lengthOfBoard
printTop (x:xs) 
    | length (x:xs) == 8 = " |" ++ printLetter x ++ printTop xs
    | otherwise = printLetter x ++ printTop xs

printEmptyRow :: Int -> [Char]
printEmptyRow 0 = endLine
printEmptyRow x = printSpaceForJumper ++ printEmptyRow (x - 1)

printLineRow :: Int -> [Char]
printLineRow 0 = endLine
printLineRow x   
    | x == lengthOfBoard = "-|" ++ printLine ++ printLineRow (x - 1)
    | otherwise = printLine ++ printLineRow (x - 1)

printSymbol :: (Integer, Char, Char) -> [Char]
printSymbol x = "|  " ++ [third x] ++ "  "

printRow :: [(Integer, Char, Char)] -> [Char]
printRow [] = endLine
printRow (x:xs) 
    | length (x:xs) == lengthOfBoard = printNumber ++ printSymbol x ++ printRow xs
    | otherwise = printSymbol x ++ printRow xs
    where printNumber = show (first x) ++ "|" 

printRest :: [[(Integer, Char, Char)]] -> [Char]
printRest [] = "=|" ++ printMultipleEquals lengthOfBoard
printRest (x:xs) = printRow x ++ (printLineRow lengthOfBoard) ++ printRest xs