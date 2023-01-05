import Printing
import Play

startGame color = do 
    putStrLn (printAll board)
   -- go board
    where board = fillBoard numbers topLetters color

main = do
    putStrLn "Wybierz swoj kolor: bialy (B), czarny (C)"
    color <- getLine
    startGame color
    --putStrLn (printRest numbers)