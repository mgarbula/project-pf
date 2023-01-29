module Oponent where
import Play
import Printing

target = map (\l -> (7, l)) topLetters ++ map (\l -> (8, l)) topLetters

isFieldEmpty :: [[Field]] -> Integer -> Integer -> Bool
isFieldEmpty board column row
    | row <= 0 || column <= 0 = False
    | row > 8 || column > 8 = False
    | not (elem column (map (\(a, b) -> b) valueOfLetters)) = False -- sprawdzam czy kolumna do ktorej chce przejsc istnieje
    | third (head (dropWhile (\c -> second c /= columnLetter) (head (dropWhile (\r -> first (head r) /= row) board)))) /= ' ' = False
    | otherwise = True
    where columnLetter = fst (head (dropWhile (\(_, val) -> val /= column) valueOfLetters))

getColumn columnValue = head (dropWhile (\(_, value) -> value /= columnValue) valueOfLetters)
getColumnValue field = snd (head (dropWhile (\(letter, _) -> letter /= second field) valueOfLetters))

moveOneForwardPossible :: [[Field]] -> [Field] -> (Bool, [Position])
moveOneForwardPossible _ [] = (False, [('Z', 9)])
moveOneForwardPossible board (x:xs)
    | isFieldEmpty board (getColumnValue x) (first x + 1) = (True, [(second x, first x), (second x, first x + 1)])
    | otherwise = moveOneForwardPossible board xs

isSomeEmpty :: [[Field]] -> Field -> (Bool, Position)
isSomeEmpty board field
    | isFieldEmpty board (columnValue + 1) row = (True, (fst (getColumn (columnValue + 1)), row))
    | isFieldEmpty board (columnValue - 1) row = (True, (fst (getColumn (columnValue - 1)), row))
    | otherwise = (False, ('Z', 9))
    where columnValue = getColumnValue field
          row = first field

moveOnePossible :: [[Field]] -> [Field] -> (Bool, [Position])
moveOnePossible _ [] = (False, [('Z', 9)])
moveOnePossible board (x:xs)
    | let isEmpty = isSomeEmpty board x,
        fst isEmpty = (True, [(second x, first x), snd isEmpty])
    | otherwise = moveOnePossible board xs

moveOneBackPossible :: [[Field]] -> [Field] -> (Bool, [Position])
moveOneBackPossible _ [] = (False, [('Z', 9)])
moveOneBackPossible board (x:xs)
    | isFieldEmpty board (getColumnValue x) (first x - 1) = (True, [(second x, first x), (second x, first x - 1)])
    | otherwise = moveOneBackPossible board xs

findField :: [[Field]] -> Position -> Field
findField board position = head (dropWhile (\f -> fst position /= second f || snd position /= first f) (foldl (++) [] board))

isSomeJumpPossible :: [[Field]] -> Field -> [Position] -> [Position]
isSomeJumpPossible board field previous
    | let newField = (fst (getColumn columnValue), row + 2), 
        isFieldEmpty board columnValue (row + 2) && not (isFieldEmpty board columnValue (row + 1)) && notElem newField previous = isSomeJumpPossible board (findField board (second field, first field + 2)) (previous ++ [newField])
    | let newField = (fst (getColumn (columnValue + 2)), row),
        isFieldEmpty board (columnValue + 2) row && not (isFieldEmpty board (columnValue + 1) row) && notElem newField previous = isSomeJumpPossible board (findField board (fst newField, first field)) (previous ++ [newField])
    | let newField = (fst (getColumn (columnValue - 2)), row),
        isFieldEmpty board (columnValue - 2) row && not (isFieldEmpty board (columnValue - 1) row) && notElem newField previous = isSomeJumpPossible board (findField board (fst newField, first field)) (previous ++ [newField])
    | otherwise = previous
    where columnValue = getColumnValue field
          row = first field

jumpForwardPossible :: [[Field]] -> [Field] -> (Bool, [Position])
jumpForwardPossible _ [] = (False, [('Z', 9)])
jumpForwardPossible board (x:xs)
    | length someJump > 1 = (True, someJump)
    | otherwise = jumpForwardPossible board xs
    where someJump = isSomeJumpPossible board x [(second x, first x)]

onlyOneJumpForward :: [[Field]] -> [Field] -> (Bool, [Position])
onlyOneJumpForward _ [] = (False, [('Z', 9)])
onlyOneJumpForward board (x:xs)
    | let newField = (fst (getColumn columnValue), row + 2), 
        isFieldEmpty board columnValue (row + 2) && not (isFieldEmpty board columnValue (row + 1)) = (True, isSomeJumpPossible board (findField board (second x, first x + 2)) ((second x, first x) : [newField]))
    | otherwise = onlyOneJumpForward board xs
    where columnValue = getColumnValue x
          row = first x