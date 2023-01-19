import Printing
import Players

startGame color = do 
    putStrLn (printAll board)
    go board color
    where board = fillBoard numbers topLetters color

chooseColor :: IO()
chooseColor = do
    putStrLn "Wybierz swoj kolor: bialy (B), czarny (C)"
    color <- getLine
    if color /= "B" && color /= "C"
        then do
            putStrLn "Podano niepoprawny kolor"
            chooseColor
    else
        startGame color

main = do
    chooseColor