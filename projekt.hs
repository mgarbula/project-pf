import Printing

startGame color = do 
    putStrLn (printTop topLetters)
    putStrLn (printRest (fillBoard numbers topLetters color))

main = do
    putStrLn "Wybierz swoj kolor: bialy (B), czarny (C)"
    color <- getLine
    startGame color
    --putStrLn (printRest numbers)