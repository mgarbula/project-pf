import Printing
import Players

-- zapętlenie przeciwnika
--board = [[(1,'A','B'),(1,'B','B'),(1,'C','B'),(1,'D','B'),(1,'E','B'),(1,'F','B'),(1,'G','B'),(1,'H','B')],[(2,'A','B'),(2,'B','B'),(2,'C','B'),(2,'D','B'),(2,'E','B'),(2,'F','B'),(2,'G','B'),(2,'H',' ')],[(3,'A',' '),(3,'B',' '),(3,'C',' '),(3,'D',' '),(3,'E',' '),(3,'F',' '),(3,'G',' '),(3,'H',' ')],[(4,'A','C'),(4,'B',' '),(4,'C','B'),(4,'D',' '),(4,'E',' '),(4,'F',' '),(4,'G',' '),(4,'H',' ')],[(5,'A',' '),(5,'B',' '),(5,'C',' '),(5,'D',' '),(5,'E',' '),(5,'F',' '),(5,'G',' '),(5,'H',' ')],[(6,'A',' '),(6,'B',' '),(6,'C',' '),(6,'D',' '),(6,'E',' '),(6,'F',' '),(6,'G',' '),(6,'H',' ')],[(7,'A','C'),(7,'B','C'),(7,'C','C'),(7,'D','C'),(7,'E','C'),(7,'F','C'),(7,'G','C'),(7,'H',' ')],[(8,'A','C'),(8,'B','C'),(8,'C','C'),(8,'D','C'),(8,'E','C'),(8,'F','C'),(8,'G','C'),(8,'H','C')]]
-- brak zapętlenia
--board = [[(1,'A','B'),(1,'B','B'),(1,'C','B'),(1,'D','B'),(1,'E','B'),(1,'F','B'),(1,'G','B'),(1,'H','B')],[(2,'A','B'),(2,'B','B'),(2,'C','B'),(2,'D','B'),(2,'E','B'),(2,'F','B'),(2,'G','B'),(2,'H',' ')],[(3,'A',' '),(3,'B',' '),(3,'C',' '),(3,'D',' '),(3,'E',' '),(3,'F',' '),(3,'G',' '),(3,'H',' ')],[(4,'A',' '),(4,'B',' '),(4,'C','B'),(4,'D',' '),(4,'E',' '),(4,'F',' '),(4,'G',' '),(4,'H','C')],[(5,'A',' '),(5,'B',' '),(5,'C',' '),(5,'D',' '),(5,'E',' '),(5,'F',' '),(5,'G',' '),(5,'H',' ')],[(6,'A',' '),(6,'B',' '),(6,'C',' '),(6,'D',' '),(6,'E',' '),(6,'F',' '),(6,'G',' '),(6,'H',' ')],[(7,'A',' '),(7,'B','C'),(7,'C','C'),(7,'D','C'),(7,'E','C'),(7,'F','C'),(7,'G','C'),(7,'H','C')],[(8,'A','C'),(8,'B','C'),(8,'C','C'),(8,'D','C'),(8,'E','C'),(8,'F','C'),(8,'G','C'),(8,'H','C')]]
-- board = [[(1,'A','B'),(1,'B','B'),(1,'C','B'),(1,'D','B'),(1,'E','B'),(1,'F','B'),(1,'G','B'),(1,'H','B')],[(2,'A','B'),(2,'B','B'),(2,'C','B'),(2,'D','B'),(2,'E','B'),(2,'F','B'),(2,'G','B'),(2,'H',' ')],[(3,'A',' '),(3,'B',' '),(3,'C',' '),(3,'D',' '),(3,'E',' '),(3,'F',' '),(3,'G',' '),(3,'H',' ')],[(4,'A',' '),(4,'B',' '),(4,'C','B'),(4,'D',' '),(4,'E',' '),(4,'F',' '),(4,'G',' '),(4,'H','C')],[(5,'A',' '),(5,'B',' '),(5,'C',' '),(5,'D',' '),(5,'E',' '),(5,'F',' '),(5,'G',' '),(5,'H',' ')],[(6,'A',' '),(6,'B',' '),(6,'C',' '),(6,'D',' '),(6,'E',' '),(6,'F',' '),(6,'G',' '),(6,'H',' ')],[(7,'A',' '),(7,'B','C'),(7,'C','C'),(7,'D','C'),(7,'E','C'),(7,'F','C'),(7,'G','C'),(7,'H','C')],[(8,'A','C'),(8,'B','C'),(8,'C','C'),(8,'D','C'),(8,'E','C'),(8,'F','C'),(8,'G','C'),(8,'H','C')]]
-- color = "B"

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