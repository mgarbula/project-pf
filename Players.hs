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
            if possibleWin myFields 1 2
                then do
                    putStrLn "Koniec gry! Wygrales!"
                else do
                    oponent newBoard color
        else do
            putStrLn "Niedozwolony ruch"
            playersMove board color

updateBoard :: [[Field]] -> String -> Char -> String -> IO()
updateBoard board move myColor color = do
    putStrLn move
    let newBoard = makeMove board move myColor
    let myFields = changePlayerField (playerFields board myColor) (fromTo move) myColor
    putStrLn (printAll newBoard)
    if possibleWin myFields 7 8
        then do
            putStrLn "Koniec gry! Wygral komputer!"
        else do
            playersMove newBoard color

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
            updateBoard board move myColor color            
    else do
        let possibleMove = jumpPossible board myFields
        if first possibleMove
            then do
                let from = second possibleMove
                let to = third possibleMove
                let move = fst from : (show (snd from) ++ ('-' : (fst to) : show (snd to)))
                updateBoard board move myColor color
        else do
            putStrLn "Brak ruchu"
            playersMove board color

go :: [[Field]] -> String -> IO ()
go board color = do
    if color == "B"
        then do
            playersMove board color
        else do
            oponent board color