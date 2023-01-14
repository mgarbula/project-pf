module Oponent where
import Play
import Printing

target = map (\l -> (7, l)) topLetters ++ map (\l -> (8, l)) topLetters

oponentFields :: [[Field]] -> Color -> [Field]
oponentFields board color = playerFields board color

isNextEmpty :: [[Field]] -> Field -> Bool
isNextEmpty board field = third (head (filter (\f -> second f == second field) row)) == ' '
    where row = head (dropWhile (\r -> first (head r) /= (first field) + 1) board)

moveForwardPossible :: [[Field]] -> [Field] -> (Bool, Position)
moveForwardPossible board [] = (False, ('Z', 9))
moveForwardPossible board (x:xs) 
    | isNextEmpty board x = (True, (second x, first x))
    | otherwise = moveForwardPossible board xs
    