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
    removePiece, isPiece
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
    piece :: Piece,
    nextPiece :: Piece,
    holdPiece :: Piece,
    isHolded :: Bool,
    winGame :: Bool,
    loseGame :: Bool,
    framesPast :: Int,
    framesNeed :: Int
} deriving (Show)

---

data Piece = Empty | Piece [[Int]] [[Int]] (Int, Int) (Int, Int) deriving (Eq, Show)

isEmpty :: Piece -> Bool
isEmpty Empty = True
isEmpty _     = False

generatePiece :: Int -> Piece
generatePiece i = Piece piece' piece' initialCoordenate finalCoordenate
    where
        (piece', initialCoordenate, finalCoordenate) = buildPiece i 

getPiece :: Piece -> [[Int]]
getPiece Empty         = []
getPiece (Piece p _ _ _) = p

---

originalState :: State
originalState = State {
    board = putPiece originalBoard firstPiecePrepared,
    totalLines = 0, level = 0, time = 0, score = 0,
    piece = piece', nextPiece = generatePiece 1, holdPiece = Empty, isHolded = False,
    loseGame = False, framesPast = 0, framesNeed = 60, winGame = False
    }
    where
        originalBoard = [[0 | _ <- [0..9]] | _ <- [0..19]]
        (firstPiece, (xo, yo), (xf, yf)) = buildPiece 0
        piece' = Piece firstPiece firstPiece (xo, yo) (xf, yf)
        firstPiecePrepared = preparePieceToPut firstPiece (xo, yo) (xf, yf)
---

incrementScore :: State -> Int -> State
incrementScore state newScore = state {score = (succ (level state) * newScore) + score state}

---

updateState :: State -> State
updateState state
    | isPossiblePutPiece (board state) newPiecePrepared =
        putPreviewState $ state {
            board = putPiece newBoard newPiecePrepared,
            nextPiece = nextPiece',
            piece = nextPiece state,
            totalLines = newTotalLines,
            level = newTotalLines `div` 10,
            score = newScore,
            isHolded = False
            }
    | otherwise = putPreviewState $ state {loseGame = True}
    where
        nextPiece' = generatePiece (calculateNext (level state) (totalLines state) (time state) (score state))
        Piece _ piece' initialCoordenate finalCoordenate = nextPiece state
        newPiecePrepared = preparePieceToPut piece' initialCoordenate finalCoordenate
        (newBoard, linesAdd) = (clearGame . freezeBoard) (board state)
        newTotalLines = totalLines state + linesAdd
        newLevel = newTotalLines `mod` 10
        newScore = linesAdd * newLevel + score state

calculateNext :: Int -> Int -> Int -> Int -> Int
calculateNext level' lines' time' score' =
    time' + lines' + score' `mod` (time' + level' + 1)

---

moveRightState :: State -> State
moveRightState state
    | isPossibleMoveRight (removePreview (board state)) = putPreviewState $ state {board = (moveRight . removePreview) (board state)}
    | otherwise = state

moveLeftState :: State -> State
moveLeftState state
    | isPossibleMoveLeft (removePreview (board state)) = putPreviewState $ state {board = (moveLeft . removePreview) (board state)}
    | otherwise = state

moveDownState :: State -> State
moveDownState state
    | isPossibleMoveDown (removePreview (board state)) = putPreviewState $ incrementScore (state {board = newBoard }) 10
    | otherwise = updateState state
    where
        newBoard = moveDown (board state)

moveSpaceState :: State -> State
moveSpaceState state
    | isPossibleMoveDown (removePreview (board state)) = updateState $ incrementScore (state {board = newBoard}) (moveDowns * 15)
    | otherwise = updateState state
    where 
        (newBoard, moveDowns) = (moveSpace . removePreview) (board state)

---

rotateRightState :: State -> State
rotateRightState state
    | isPossiblePutPiece ((removePiece . removePreview) (board state)) pieceRotatedPrepared =
        putPreviewState $ state {
            board = putPiece ((removePreview . removePiece) (board state)) pieceRotatedPrepared,
            piece = Piece original pieceRotated initialCoordenate finalCoordenate
        }
    | otherwise = putPreviewState $ state
    where
        Piece original piece' initialCoordenate finalCoordenate = piece state
        pieceRotated = rotatePieceRight piece'
        ((xo, yo), (xf, yf)) = discoverCoordenates (board state)
        pieceRotatedPrepared = preparePieceToPut pieceRotated (xo, yo) (xf, yf)


rotateLeftState :: State -> State
rotateLeftState state
    | isPossiblePutPiece ((removePiece . removePreview) (board state)) pieceRotatedPrepared =
        putPreviewState $ state {
            board = putPiece ((removePreview . removePiece) (board state)) pieceRotatedPrepared,
            piece = Piece original pieceRotated initialCoordenate finalCoordenate
        }
    | otherwise = putPreviewState state
    where
        Piece original piece' initialCoordenate finalCoordenate = piece state
        pieceRotated = rotatePieceLeft piece'
        ((xo, yo), (xf, yf)) = discoverCoordenates (board state)
        pieceRotatedPrepared = preparePieceToPut pieceRotated (xo, yo) (xf, yf)

---

holdPieceState :: State -> State
holdPieceState state
    | isHolded state = state
    | isEmpty (holdPiece state) = updateHoldStatus $ updateState state {
        board = removePiece (board state),
        holdPiece = piece state
        } 
    | otherwise = putPreviewState $ state {
        board = putPiece (removePiece (board state)) piecePrepared,
        holdPiece = piece state,
        piece = Piece original original initialCoordenate finalCoordenate,
        isHolded = True }
    where
        Piece original _ initialCoordenate finalCoordenate = holdPiece state
        piecePrepared = preparePieceToPut original initialCoordenate finalCoordenate 

updateHoldStatus :: State -> State
updateHoldStatus state = state {isHolded = True}

---

putPreviewState  :: State -> State
putPreviewState  state = state {
    board = putPiece (board state) pieceRetired
    }
    where
        (boardPreview, _) = (moveSpace . removePreview) (board state)
        pieceRetired      = (substitute . removePieceFromBoard) boardPreview

removePreview :: [[Int]] -> [[Int]]
removePreview = map (map deletePreview)

deletePreview :: Int -> Int
deletePreview 18 = 0
deletePreview p  = p 

substitute :: [[Int]] -> [[Int]]
substitute = map (map putCollorPreview)

putCollorPreview :: Int -> Int
putCollorPreview p 
    | isPiece p = 18
    | otherwise = p

removePieceFromBoard :: [[Int]] -> [[Int]]
removePieceFromBoard = map (map tradePiece) 

tradePiece :: Int -> Int
tradePiece p
    | isPiece p = p
    | otherwise = 0