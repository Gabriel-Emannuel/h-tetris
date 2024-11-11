module Components where

import Graphics.Gloss.Interface.IO.Game

renderBoard :: [[Int]] -> (Float, Float) -> Picture
renderBoard board (xP, yP) = translate xP yP $ pictures $ zipWith (curry renderLine) (reverse board) [0..]
  where
    renderLine (row, y) = pictures $ zipWith (\ cell x -> renderCell cell x y) row [0..]
    renderCell cell x y = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ color (colorForCell cell) $ rectangleSolid cellSize cellSize
    cellSize = 30
    colorForCell cell
        | (cell `mod` 10) == 0 = black
        | (cell `mod` 10) == 1 = blue
        | (cell `mod` 10) == 2 = orange
        | (cell `mod` 10) == 3 = yellow
        | (cell `mod` 10) == 4 = dark blue
        | (cell `mod` 10) == 5 = green
        | (cell `mod` 10) == 6 = violet
        | (cell `mod` 10) == 7 = red
        | otherwise = error "Color doesn't exist"

renderNextPiece :: [[Int]] -> (Float, Float) -> (Float, Float) -> String -> Picture
renderNextPiece piece (xtext'', ytext'') (xpiece, ypiece) text' = pictures [nextPiece, text'']
  where
    nextPiece = renderBoard (reverse piece) (xpiece, ypiece)
    text'' = translate xtext'' ytext'' $ scale 0.2 0.2 $ color black $ text text'

renderText :: String -> String -> (Float, Float, Float, Float) -> Picture
renderText text1 text2 (xPosition, yPosition, height, width) = 
    Translate xPosition yPosition $ Color black $ Scale height width $ Text (text1 ++ " " ++text2)