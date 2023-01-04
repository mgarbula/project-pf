topLetters = ['A'..'H']
numbers = [1..8]
lengthOfBoard = 8
printSpaceForJumper = "|     "
printLine = "|-----"
printEquals = "|====="
endLine = "||\n"

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
printLineRow x = printLine ++ printLineRow (x - 1)

printRest :: [Int] -> [Char]
printRest [] = "=|" ++ printMultipleEquals lengthOfBoard
printRest (x:xs)
    | length (x:xs) == lengthOfBoard = printNumber
    | otherwise = " |" ++ printLineRow lengthOfBoard ++ printNumber
    where printNumber = show x ++ "|" ++ printEmptyRow lengthOfBoard ++ printRest xs

main = do
    putStrLn (printTop topLetters)
    putStrLn (printRest numbers)