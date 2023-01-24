module Oponent where
import Play
import Printing

target = map (\l -> (7, l)) topLetters ++ map (\l -> (8, l)) topLetters

isNextEmpty :: [[Field]] -> Field -> Bool
isNextEmpty board field  
    | elem (first field + 1) numbers == False = False
    | otherwise = third (head (filter (\f -> second f == second field) row)) == ' '
    where row = head (dropWhile (\r -> first (head r) /= (first field) + 1) board)

moveForwardPossible :: [[Field]] -> [Field] -> (Bool, Position)
moveForwardPossible _ [] = (False, ('Z', 9))
moveForwardPossible board (x:xs) 
    | isNextEmpty board x = (True, (second x, first x))
    | otherwise = moveForwardPossible board xs

isFieldEmpty :: [[Field]] -> Integer -> Integer -> Bool
isFieldEmpty board column row
    | row <= 0 || column <= 0 = False
    | row > 8 || column > 8 = False
    | elem column (map (\(a, b) -> b) valueOfLetters) == False = False -- sprawdzam czy kolumna do ktorej chce przejsc istnieje
    | third (head (dropWhile (\c -> second c /= columnLetter) (head (dropWhile (\r -> first (head r) /= row) board)))) /= ' ' = False
    | otherwise = True
    where columnLetter = fst (head (dropWhile (\(_, val) -> val /= column) valueOfLetters))

getColumn columnValue = head (dropWhile (\(_, value) -> value /= columnValue + 1) valueOfLetters)
isSomeJumpPossible :: [[Field]] -> Field -> (Bool, Position, Position)
isSomeJumpPossible board field 
    | isFieldEmpty board (columnValue + 1) (row + 2) = (True, (second field, row), (fst (getColumn columnValue), row + 2))
    | isFieldEmpty board (columnValue - 2) (row + 1) = (True, (second field, row), (fst (getColumn (columnValue - 3)), row + 1)) -- getColumn dodaje 1, więc musze odjąc 3
    | isFieldEmpty board (columnValue - 1) (row + 2) = (True, (second field, row), (fst (getColumn (columnValue - 2)), row + 2))
    | isFieldEmpty board (columnValue + 2) (row + 1) = (True, (second field, row), (fst (getColumn (columnValue + 1)), row + 1))
    | otherwise = (False, ('Z', 9), ('Z', 9))
    where columnValue = snd (head (dropWhile (\(letter, _) -> letter /= second field) valueOfLetters))
          row = first field

jumpForwardPossible :: [[Field]] -> [Field] -> (Bool, Position, Position)
jumpForwardPossible _ [] = (False, ('Z', 9), ('Z', 9))
jumpForwardPossible board (x:xs)
    | first someJump = someJump
    | otherwise = jumpForwardPossible board xs
    where someJump = isSomeJumpPossible board x

isSomeJumpBackPossible :: [[Field]] -> Field -> (Bool, Position, Position)
isSomeJumpBackPossible board field
    | isFieldEmpty board (columnValue - 2) (row - 1) = (True, (second field, row), (fst (getColumn (columnValue - 3)), row - 1))
    | isFieldEmpty board (columnValue + 2) (row - 1) = (True, (second field, row), (fst (getColumn (columnValue + 1)), row - 1))
    | isFieldEmpty board (columnValue - 1) (row - 2) = (True, (second field, row), (fst (getColumn (columnValue - 2)), row - 2))
    | isFieldEmpty board (columnValue + 1) (row - 2) = (True, (second field, row), (fst (getColumn columnValue), row - 2))
    | otherwise = (False, ('Z', 9), ('Z', 9))
    where columnValue = snd (head (dropWhile (\(letter, _) -> letter /= second field) valueOfLetters))
          row = first field

jumpBackPossible :: [[Field]] -> [Field] -> (Bool, Position, Position)
jumpBackPossible _ [] = (False, ('Z', 9), ('Z', 9))
jumpBackPossible board (x:xs)
    | first someJump = someJump
    | otherwise = jumpBackPossible board xs
    where someJump = isSomeJumpBackPossible board x