module Pieces (
    isEmpty, isFreeze, isPiece,
    buildPiece,
    rotatePieceLeft, rotatePieceRight,
    preparePieceToPut, isPossiblePutPiece, removePiece, putPiece,
    freezeBoard, 
    discoverCoordenates
) where

import Data.List (transpose, findIndex)
import Data.Maybe (mapMaybe)

---

isEmpty :: Int -> Bool
isEmpty 0 = True
isEmpty _ = False

isFreeze :: Int -> Bool
isFreeze p = p > 9

isPiece :: Int -> Bool
isPiece p = p > 0 && p < 10

---
buildPiece :: Int -> ([[Int]], (Int, Int), (Int, Int))
buildPiece i = 
    [buildI, buildL, buildO, buildR, buildS, buildT, buildZ] !! (i `mod` 7)

buildI :: ([[Int]], (Int, Int), (Int, Int))
buildI = ([
    [1,1,1,1],
    [0,0,0,0],
    [0,0,0,0],
    [0,0,0,0]
    ], (3,0), (6,0))

buildL :: ([[Int]], (Int, Int), (Int, Int))
buildL = ([
    [2,0,0],
    [2,2,2],
    [0,0,0]
    ], (4,0), (6,2))

buildO :: ([[Int]], (Int, Int), (Int, Int))
buildO = ([
    [3,3],
    [3,3]
    ], (4,0), (5,1))

buildR :: ([[Int]], (Int, Int), (Int, Int))
buildR = ([
    [0,0,4],
    [4,4,4],
    [0,0,0]
    ], (4,0), (6,2))

buildS :: ([[Int]], (Int, Int), (Int, Int))
buildS = ([
    [0,5,5],
    [5,5,0],
    [0,0,0]
    ], (4,0), (6,2))

buildT :: ([[Int]], (Int, Int), (Int, Int))
buildT = ([
    [0,6,0],
    [6,6,6],
    [0,0,0]
    ], (4,0), (6,2))

buildZ :: ([[Int]], (Int, Int), (Int, Int))
buildZ = ([
    [7,7,0],
    [0,7,7],
    [0,0,0]
    ], (4,0), (6,2))

---

rotatePieceLeft :: [[Int]] -> [[Int]]
rotatePieceLeft = transpose . map reverse

rotatePieceRight :: [[Int]] -> [[Int]]
rotatePieceRight = map reverse . transpose

---

preparePieceToPut :: [[Int]] -> (Int, Int) -> (Int, Int) -> [[Int]]
preparePieceToPut piece (xo, yo) (xf, yf) =
    [newLine | _ <- [0..pred yo]] ++ newPiece ++ [newLine | _ <- [succ yf..19]]
    where
        newLine = [0 | _ <- [0..9]]
        newPiece = map ((\line -> [0 | _ <- [0..pred xo]] ++ line) . (\line -> line ++ [0 | _ <- [succ xf .. 9]])) piece

isPossiblePutPiece :: [[Int]] -> [[Int]] -> Bool
isPossiblePutPiece board piece = all and validPoints
    where
        validPoints = zipWith (zipWith (\pBoard pPiece -> not (isPiece pPiece) || isEmpty pBoard)) board piece

--

removePiece :: [[Int]] -> [[Int]]
removePiece = map (map remove) 

remove :: Int -> Int
remove p 
    | isPiece p = 0
    | otherwise = p

--
putPiece :: [[Int]] -> [[Int]] -> [[Int]]
putPiece = zipWith (zipWith put)

put :: Int -> Int -> Int
put 0 piece = piece
put piece _ = piece

---

freezeBoard :: [[Int]] -> [[Int]]
freezeBoard = map (map freeze)

freeze :: Int -> Int
freeze piece
    | isPiece piece = piece + 10
    | otherwise     = piece

---

discoverCoordenates :: [[Int]] -> ((Int, Int), (Int, Int))
discoverCoordenates board = ((xo, yo), (xf, yf))
    where
        (yo', yf') = findVerticalCoordenates board
        (xo', xf') = findHorizontalCoordenates board
        ((xo, yo), (xf, yf)) = findCoordenates (xo', yo') (xf', yf')

--

findCoordenates :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
findCoordenates (xo, yo) (xf, yf)
    | dimensionX == dimensionY = ((xo, yo), (xf, yf))
    | dimensionX < dimensionY  = ((xo', yo), (xf', yf))
    | otherwise                = ((xo, yo'), (xf, yf'))
    where
        dimensionX = xf - xo
        dimensionY = yf - yo
        (xo', xf') = fixHorizontalCoordenates (xo, xf) (dimensionY - dimensionX)
        (yo', yf') = fixVerticalCoordenates   (yo, yf) (dimensionX - dimensionY)

fixVerticalCoordenates :: (Int, Int) -> Int -> (Int, Int)
fixVerticalCoordenates (yo, yf) diff
    | yf + diff > 19 = (yo - diff, yf)
    | otherwise      = (yo, yf + diff)

fixHorizontalCoordenates :: (Int, Int) -> Int -> (Int, Int)
fixHorizontalCoordenates (xo, xf) diff
    | xf + diff > 9 = (xo - diff, xf)
    | otherwise     = (xo, xf + diff) 

--

findHorizontalCoordenates :: [[Int]] -> (Int, Int)
findHorizontalCoordenates board = (xo, 9 - xf)
    where
        xCoordenates = mapMaybe (findIndex isPiece) board
        xCoordenates' = mapMaybe (findIndex isPiece . reverse) board
        xo = minimum xCoordenates
        xf = minimum xCoordenates'

findVerticalCoordenates :: [[Int]] -> (Int, Int)
findVerticalCoordenates board = (yo, 19 - yf)
    where
        containsPieces = map (filter isPiece) board
        Just yo = findIndex (not . null) containsPieces
        Just yf = findIndex (not . null) (reverse containsPieces)