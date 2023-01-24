module Printing where

topLetters = ['A'..'H']
numbers = [1..8]
valueOfLetters = zip topLetters numbers
lengthOfBoard = 8
printSpaceForJumper = "|     "
printLine = "|-----"
printEquals = "|====="
endLine = "||\n"

type Field = (Integer, Char, Char)

oponentColor :: String -> Char
oponentColor c
    | c == "B" = 'C'
    | otherwise = 'B'

myColor :: String -> Char
myColor c
    | c == "B" = 'B'
    | otherwise = 'C'

fillBoard :: [Integer] -> [Char] -> String -> [[Field]] -- pozycja i znak ktory ma byc drukowany
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
printLetter x = "|  " ++ [x] ++ "  "

printMultipleEquals :: Int -> [Char]
printMultipleEquals 0 = "||=\n" 
printMultipleEquals x = printEquals ++ printMultipleEquals (x - 1)

printSeparator = "=|" ++ printMultipleEquals lengthOfBoard 

printLetters :: [Char] -> [Char]    
printLetters [] = endLine
printLetters (x:xs) 
    | length (x:xs) == 8 = " |" ++ printLetter x ++ printLetters xs
    | otherwise = printLetter x ++ printLetters xs

printEmptyRow :: Int -> [Char]
printEmptyRow 0 = endLine
printEmptyRow x = printSpaceForJumper ++ printEmptyRow (x - 1)

printLineRow :: Int -> [Char]
printLineRow 0 = "||-\n"
printLineRow x   
    | x == lengthOfBoard = "-|" ++ printLine ++ printLineRow (x - 1)
    | otherwise = printLine ++ printLineRow (x - 1)

printSymbol :: Field -> [Char]
printSymbol x = "|  " ++ [third x] ++ "  "

printRow :: [Field] -> Integer -> [Char]
printRow [] x = "||" ++ show x ++ "\n"
printRow (x:xs) num
    | length (x:xs) == lengthOfBoard = printNumber ++ "|" ++ printSymbol x ++ printRow xs (first x)
    | otherwise = printSymbol x ++ printRow xs (first x)
    where printNumber = show (first x)  

printRest :: [[Field]] -> [Char]
printRest [] = ""
printRest (x:xs)
    | length (x:xs) == 1 = printRow x 0 ++ "=|" ++ printMultipleEquals lengthOfBoard
    | otherwise = printRow x 0 ++ (printLineRow lengthOfBoard) ++ printRest xs

printAll :: [[Field]] -> [Char] 
printAll board = printLetters topLetters ++ printSeparator ++ printRest board ++ printLetters topLetters