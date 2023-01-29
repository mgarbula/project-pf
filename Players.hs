module Players where
import Play
import Oponent
import Printing

playersMove :: [[Field]] -> String -> IO()
playersMove board color = do
    putStrLn "Twoj ruch"
    move <- getLine
    if move == ""
        then do
            putStrLn "Nie podano ruchu"
            playersMove board color
        else do
            if possibilityOfMove board move (myColor color)
                then do
                    let playerColor = myColor color
                    let newBoard = makeMove board move playerColor
                    let myMove = fromTo (moves move)
                    let myFields = changePlayerField (playerFields board playerColor) (head myMove, last myMove) playerColor
                    putStrLn (printAll newBoard)
                    if possibleWin myFields 1 2
                        then do
                            putStrLn "Koniec gry! Wygrales!"
                        else do
                            oponent newBoard color
                else do
                    putStrLn "Niedozwolony ruch"
                    playersMove board color

makeMoveString :: [Position] -> String
makeMoveString [] = ""
makeMoveString (x:xs) = "-" ++ [fst x] ++ show (snd x) ++ makeMoveString xs

updateBoard :: [[Field]] -> [Position] -> Char -> String -> IO()
updateBoard board move myColor color = do
    putStrLn (tail (makeMoveString move))
    let myMove = [head move, last move]
    let newBoard = makeMove board (tail (makeMoveString myMove)) myColor
    let myFields = changePlayerField (playerFields board myColor) (head myMove, last myMove) myColor
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
    let myFields = playerFields board myColor
    let possibleMove = onlyOneJumpForward board myFields
    if fst possibleMove
        then do
            updateBoard board (snd possibleMove) myColor color
    else do
        let possibleMove = moveOneForwardPossible board myFields
        if fst possibleMove
            then do
                updateBoard board (snd possibleMove) myColor color
        else do
            let possibleMove = jumpForwardPossible board myFields
            if fst possibleMove
                then do
                    updateBoard board (snd possibleMove) myColor color
            else do
                let possibleMove = moveOnePossible board myFields
                if fst possibleMove
                    then do
                        updateBoard board (snd possibleMove) myColor color
                else do
                    let possibleMove = moveOneBackPossible board myFields
                    if fst possibleMove
                        then do
                            updateBoard board (snd possibleMove) myColor color
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