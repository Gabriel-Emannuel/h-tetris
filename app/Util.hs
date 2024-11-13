module Util (clearGame, complementPiece) where

clearGame :: [[Int]] -> ([[Int]], Int)
clearGame board = (boardCleared, length linesCleared)
    where
        linesCleared = filter (notElem 0) board
        boardCleared = [[0 | _ <- [0..9]] | _ <- [1..length linesCleared]] ++ filter (elem 0) board

complementPiece :: [[Int]] -> [[Int]]
complementPiece piece =  replicate leftAmmount newLine ++ map putLefts piece 
    where
        leftAmmount = 4 - length piece
        newLine = [0 | _ <- [1..4]]
        putLefts line = [0 | _ <- [1..leftAmmount]] ++ line