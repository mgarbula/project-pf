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

fillBoard :: [Integer] -> [Char] -> String -> [[(Integer, Char, Char)]]
fillBoard [] _ _ = []
fillBoard (n:ns) l c 
    | n == 1 || n == 2 = (map (\x -> (n, x, oponentColor c)) l) : fillBoard ns l c
    | n == 7 || n == 8 = (map (\x -> (n, x, myColor c)) l) : fillBoard ns l c
    | otherwise = (map (\x -> (n, x, ' ')) l) : fillBoard ns l c

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

printLetter :: Char -> [Char]
printLetter x = "| " ++ show x ++ " "

printMultipleEquals :: Int -> [Char]
printMultipleEquals 0 = endLine 
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

printAll :: [[(Integer, Char, Char)]] -> [Char] 
printAll rest = printTop topLetters ++ printRest rest