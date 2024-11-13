module Game (game) where

import Graphics.Gloss

import Components (renderBoard, renderText, renderNextPiece)

import Util (complementPiece)

import State (originalState, State(..), rotateLeftState, moveLeftState, moveRightState, moveDownState, moveSpaceState, rotateRightState, incrementScore, getPiece, holdPieceState, isOverState)

import Graphics.Gloss.Interface.IO.Game (playIO, Event (EventKey), Key (Char, SpecialKey), KeyState (Down), SpecialKey (KeyLeft, KeyRight, KeyDown, KeySpace))
import Data.Char (toLower)

game :: IO ()
game = playIO (InWindow "Tetris" (1200, 800) (10,10)) white 60 originalState renderMain inputKeyboard updateGame

---

inputKeyboard :: Event -> State -> IO State

inputKeyboard (EventKey (Char t) Down _ _ ) state
  | t' == 'r' = return originalState
  | isOverState state = return state
  | t' == 'z' = return $ rotateLeftState state
  | t' == 'x' = return $ rotateRightState state
  | t' == 'c' = return $ holdPieceState state
  | otherwise = return state
  where
    t' = toLower t

inputKeyboard (EventKey (SpecialKey tecla) Down _ _) state
  | isOverState state = return state
  | tecla == KeyLeft = return $ moveLeftState state
  | tecla == KeyRight = return $ moveRightState state
  | tecla == KeyDown = return $ incrementScore (moveDownState state) 1
  | tecla == KeySpace = return $ moveSpaceState state
  | otherwise = return state

inputKeyboard _ state = return state
---

updateGame :: Float -> State -> IO State
updateGame t state = if t == 1/60 then changeState else return state
  where
    stateDown = moveDownState state
    changeState
      | level state == 10 = return state {winGame = True}
      | loseGame state = return state
      | (framesPast state + 1) `mod` framesNeed state /= 0 =
         return state { framesPast = framesPast state + 1, time = (framesPast state + 1) `div` 60 }
      | otherwise = return stateDown {
            time = (framesPast state + 1) `div` 60,
            framesPast = framesPast state + 1,
            framesNeed = 60 - (level state * 5)
            }
---

renderMain :: State -> IO Picture
renderMain state = return $ pictures [
  boxBoard, boxLevel, boxTotalLines, boxScore,
  boxTime ,boxNextPiece, boxOver,
  boxCommandA, boxCommandD, boxCommandS, boxCommandR,
  boxCommandK, boxCommandL, boxHoldPiece, boxCommandX]
  where
    boxBoard = renderBoard (board state) (-150, -300)
    boxLevel = renderText "Level" (show (level state)) (150, -200, 0.2, 0.2)
    boxTotalLines = renderText "Lines" (show (totalLines state)) (150, -250, 0.2, 0.2)
    boxScore = renderText "Score" (show (score state)) (150, -150, 0.2, 0.2)
    boxTime = renderText "Time" (show (time state)) (150, -300, 0.2, 0.2)
    boxNextPiece = renderNextPiece ((complementPiece . reverse) (getPiece (nextPiece state))) (300, -20) (180, -60) "Next Piece"
    boxHoldPiece = renderNextPiece ((complementPiece. reverse) (getPiece (holdPiece state))) (300, 120) (180, 120) "Hold Piece" 
    boxCommandA = renderText "Left Arrow - Move Left" "" (-420, 250, 0.1, 0.1)
    boxCommandD = renderText "Right Arrow - Move Right" "" (-420, 200, 0.1, 0.1)
    boxCommandS = renderText "Down Arrow - Move Down" "" (-420, 150, 0.1, 0.1)
    boxCommandR = renderText "R - Restart Game" "" (-420, 100, 0.1, 0.1)
    boxCommandK = renderText "Z - Rotate Left" "" (-420, 50, 0.1, 0.1)
    boxCommandL = renderText "X - Rotate Right" "" (-420, 1, 0.1, 0.1)
    boxCommandX = renderText "Space - Put the piece down" "" (-420, -50, 0.1, 0.1)
    boxOver
      | loseGame state = renderText "You" "Lose" (-75, 300, 0.2, 0.2)
      | winGame state = renderText "You" "Win" (-75, 300, 0.2, 0.2)
      | otherwise = renderText "" "" (150, -350, 0.2, 0.2)