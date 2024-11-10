module Controls (
    isPossibleMoveLeft,
    isPossibleMoveRight,
    isPossibleMoveDown,
    moveLeft,
    moveRight,
    moveDown,
) where

import Pieces (isPiece, isFreeze)

isPossibleMoveLeft :: [[Int]] -> Bool
isPossibleMoveLeft = all (\line -> (not . isPiece) (head line) && isPossibleMoveLeftLine line)

isPossibleMoveLeftLine :: [Int] -> Bool
isPossibleMoveLeftLine [] = True
isPossibleMoveLeftLine [_] = True
isPossibleMoveLeftLine (c:m:r) =
    not (isFreeze c && isPiece m) && isPossibleMoveLeftLine (m:r)

---

isPossibleMoveRight :: [[Int]] -> Bool
isPossibleMoveRight = all isPossibleMoveRightLine

isPossibleMoveRightLine :: [Int] -> Bool
isPossibleMoveRightLine [] = True
isPossibleMoveRightLine [p] = (not . isPiece) p
isPossibleMoveRightLine (c:m:r) =
    not (isPiece c && isFreeze m) && isPossibleMoveRightLine (m:r)

---

isPossibleMoveDown :: [[Int]] -> Bool
isPossibleMoveDown board =
    isPossibleMoveDownLine (tail board) (init board) && isPossibleMoveDownLastLine (last board)

isPossibleMoveDownLine :: [[Int]] -> [[Int]] -> Bool
isPossibleMoveDownLine tailBoard initBoard = all and $ zipWith (zipWith isPossibleMoveDownCell) tailBoard initBoard

isPossibleMoveDownCell :: Int -> Int -> Bool
isPossibleMoveDownCell p1 p2 = not (isFreeze p2 && isPiece p1)

isPossibleMoveDownLastLine :: [Int] -> Bool
isPossibleMoveDownLastLine = not . any isPiece

---

moveLeft :: [[Int]] -> [[Int]]
moveLeft = map moveLeftLine

moveLeftLine :: [Int] -> [Int]
moveLeftLine [] = []
moveLeftLine [p] = [p]
moveLeftLine (p:m:r)
    | isPiece m = m : moveLeftLine (p:r)
    | otherwise = p : moveLeftLine (m:r)

---

moveRight :: [[Int]] -> [[Int]]
moveRight = map moveRightLine

moveRightLine :: [Int] -> [Int]
moveRightLine [] = []
moveRightLine [p] = [p]
moveRightLine (x:xs) = swapRight (x : moveRightLine xs) 

swapRight :: [Int] -> [Int]
swapRight (c:m:r)
    | isPiece c = m:c:r
    | otherwise = c:m:r
swapRight _ = error "This shouldn't happen"

---

moveDown :: [[Int]] -> [[Int]]
moveDown board =
    map clearBoard (head board) : zipWith (zipWith moveDownCell) (init board) (tail board)

clearBoard :: Int -> Int
clearBoard p
    | isPiece p = 0
    | otherwise = p

moveDownCell :: Int -> Int -> Int
moveDownCell upper lower 
    | isPiece upper = upper
    | otherwise     = lower