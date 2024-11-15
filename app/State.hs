module State where

import Pieces (
    buildPiece,
    freezeBoard,
    preparePieceToPut,
    isPossiblePutPiece,
    putPiece,
    rotatePieceLeft,
    rotatePieceRight,
    discoverCoordenates,
    removePiece
    )

import ClearGame (clearGame)

import Controls (
    isPossibleMoveDown, moveDown, moveSpace,
    isPossibleMoveLeft, moveLeft,
    isPossibleMoveRight, moveRight)

data State = State {
    board :: [[Int]],
    totalLines :: Int,
    time  :: Int,
    level :: Int,
    score :: Int,
    piece :: [[Int]],
    nextPiece :: Int,
    winGame :: Bool,
    loseGame :: Bool,
    framesPast :: Int,
    framesNeed :: Int,
    nextPiece' :: [[Int]]
} deriving (Show)

---

originalState :: State
originalState = State {
    board = putPiece originalBoard firstPiecePrepared,
    totalLines = 0, level = 1, time = 0, score = 0,
    piece = firstPiece, nextPiece = 1, loseGame = False,
    framesPast = 0, framesNeed = 60, winGame = False,
    nextPiece' = nextPiece''
    }
    where
        originalBoard = [[0 | _ <- [0..9]] | _ <- [0..19]]
        (firstPiece, (xo, yo), (xf, yf)) = buildPiece 0
        firstPiecePrepared = preparePieceToPut firstPiece (xo, yo) (xf, yf)
        (nextPiece'', _, _) = buildPiece 1
---

incrementScore :: State -> Int -> State
incrementScore state newScore = state {score = (level state * newScore) + score state}

---

updateState :: State -> State
updateState state
    | isPossiblePutPiece (board state) newPiecePrepared =
        state {
            board = putPiece newBoard newPiecePrepared,
            nextPiece = calculateNext (level state) (totalLines state) (time state) (score state),
            piece = newPiece,
            nextPiece' = nextPiece'',
            totalLines = newTotalLines,
            level = newTotalLines `div` 10,
            score = newScore
            }
    | otherwise = state {loseGame = True}
    where
        (nextPiece'', _, _) = buildPiece (calculateNext (level state) (totalLines state) (time state) (score state))
        (newPiece, (xo, yo), (xf, yf)) = buildPiece (nextPiece state)
        newPiecePrepared = preparePieceToPut newPiece (xo,yo) (xf,yf)
        (newBoard, linesAdd) = (clearGame . freezeBoard) (board state)
        newTotalLines = totalLines state + linesAdd
        newLevel = newTotalLines `mod` 10
        newScore = linesAdd * newLevel + score state

calculateNext :: Int -> Int -> Int -> Int -> Int
calculateNext level' lines' time' score' =
    time' + lines' + score' `mod` (time' + level')

---

moveRightState :: State -> State
moveRightState state
    | isPossibleMoveRight (board state) = state {board = moveRight (board state)}
    | otherwise = state

moveLeftState :: State -> State
moveLeftState state
    | isPossibleMoveLeft (board state) = state {board = moveLeft (board state)}
    | otherwise = state

moveDownState :: State -> State
moveDownState state
    | isPossibleMoveDown (board state) = incrementScore (state {board = moveDown (board state)}) 10
    | otherwise = updateState state

moveSpaceState :: State -> State
moveSpaceState state
    | isPossibleMoveDown (board state) = updateState $ incrementScore (state {board = newBoard}) (moveDowns * 15)
    | otherwise = updateState state
    where 
        (newBoard, moveDowns) = moveSpace (board state)

---

rotateRightState :: State -> State
rotateRightState state
    | isPossiblePutPiece (removePiece (board state)) pieceRotatedPrepared =
        state {
            board = putPiece (removePiece (board state)) pieceRotatedPrepared,
            piece = pieceRotated
        }
    | otherwise = state
    where
        pieceRotated = rotatePieceRight (piece state)
        ((xo, yo), (xf, yf)) = discoverCoordenates (board state)
        pieceRotatedPrepared = preparePieceToPut pieceRotated (xo, yo) (xf, yf)


rotateLeftState :: State -> State
rotateLeftState state
    | isPossiblePutPiece (removePiece (board state)) pieceRotatedPrepared =
        state {
            board = putPiece (removePiece (board state)) pieceRotatedPrepared,
            piece = pieceRotated
        }
    | otherwise = state
    where
        pieceRotated = rotatePieceLeft (piece state)
        ((xo, yo), (xf, yf)) = discoverCoordenates (board state)
        pieceRotatedPrepared = preparePieceToPut pieceRotated (xo, yo) (xf, yf)