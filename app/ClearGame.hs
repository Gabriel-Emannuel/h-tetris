module ClearGame (clearGame) where

clearGame :: [[Int]] -> ([[Int]], Int)
clearGame board = (boardCleared, length linesCleared)
    where
        linesCleared = filter (notElem 0) board
        boardCleared = [[0 | _ <- [0..9]] | _ <- [1..length linesCleared]] ++ filter (elem 0) board