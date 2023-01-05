import Printing

startGame color = do 
    putStrLn (printTop topLetters)
    putStrLn (printRest (makeBoard numbers topLetters color))

main = do
    putStrLn "Wybierz swoj kolor: bialy (B), czarny (C)"
    color <- getLine
    startGame color
    --putStrLn (printRest numbers)