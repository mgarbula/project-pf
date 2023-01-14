module Players where
import Play
import Oponent
import Printing

playersMove :: [[Field]] -> String -> IO()
playersMove board color = do
    putStrLn "Twoj ruch"
    move <- getLine
    if possibilityOfMove board move
        then do
            let playerColor = myColor color
            let newBoard = makeMove board move playerColor
            let myFields = changePlayerField (playerFields board playerColor) (fromTo move) playerColor
            putStrLn (printAll newBoard)
            if possibleWinOfPlayer myFields
                then do
                    putStrLn "Koniec gry! Wygrales!"
                else do
                    oponent newBoard color
        else do
            putStrLn "Niedozwolony ruch"
            playersMove board color

oponent :: [[Field]] -> String -> IO()
oponent board color = do
    putStrLn "Ruch przeciwnika"
    let myColor = oponentColor color
    let myFields = oponentFields board myColor
    let possibleMove = moveForwardPossible board myFields
    if fst possibleMove
        then do
            let position = snd possibleMove
            let move = fst position : (show (snd position) ++ ('-' : (fst position) : show (snd position + 1)))
            putStrLn move
            let newBoard = makeMove board move myColor
            let myFields = changePlayerField (playerFields board myColor) (fromTo move) myColor
            putStrLn (printAll newBoard)
            if possibleWinOfPlayer myFields
                then do
                    putStrLn "Koniec gry! Wygrales!"
                else do
                    playersMove newBoard color
        else do
            putStr ""


go :: [[Field]] -> String -> IO ()
go board color = do
    if color == "B"
        then do
            playersMove board color
        else do
            oponent board color