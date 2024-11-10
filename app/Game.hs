module Game (game) where

import Graphics.Gloss

import Components (renderBoard, renderText, renderNextPiece)

import State (originalState, State(..), rotateLeftState, moveLeftState, moveRightState, moveDownState, rotateRightState)

import Graphics.Gloss.Interface.IO.Game (playIO, Event (EventKey), Key (Char, SpecialKey), KeyState (Down), SpecialKey (KeyLeft, KeyRight, KeyDown, KeySpace))
import Data.Char (toLower)
import Data.List (nub)

game :: IO ()
game = playIO (InWindow "Tetris" (1200, 800) (10,10)) white 60 originalState renderMain inputKeyboard updateGame

---

inputKeyboard :: Event -> State -> IO State

inputKeyboard (EventKey (Char t) Down _ _ ) state
  | t' /= 'r' = return state
  | t' == 'z' = return $ rotateLeftState state
  | t' == 'x' = return $ rotateRightState state
  | otherwise = return state
  where
    t' = toLower t

inputKeyboard (EventKey (SpecialKey tecla) Down _ _) state
  | tecla == KeyLeft = return $ moveLeftState state
  | tecla == KeyRight = return $ moveRightState state
  | tecla == KeyDown = return $ moveDownState state
  | tecla == KeySpace = return state
  | otherwise = return state
inputKeyboard _ state = return state

---

updateGame :: Float -> State -> IO State
updateGame t state = if t == 1/60 then changeState else return state
  where
    stateDown = moveDownState state
    -- (gridLimpa, qtdTotalLinesLimpas) = clearGame (grid state)
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
  boxCommandK, boxCommandL, boxCommandX]
  where
    boxBoard = renderBoard (board state) (-150, -300)
    boxLevel = renderText "Level" (show (level state)) (150, -200, 0.2, 0.2)
    boxTotalLines = renderText "TotalLines" (show (totalLines state)) (150, -250, 0.2, 0.2)
    boxScore = renderText "Score" (show (score state)) (150, -150, 0.2, 0.2)
    boxTime = renderText "Time" (show (time state)) (150, -300, 0.2, 0.2)
    boxNextPiece = renderNextPiece (filter (\line -> length (nub line) == 1 && (head . nub) line /= 0) (piece state)) (150, -20) (180, -60)
    boxCommandA = renderText "seta pra esquerda mover pra esquerda" "" (-420, 250, 0.1, 0.1)
    boxCommandD = renderText "seta pra direita mover pra direita" "" (-420, 200, 0.1, 0.1)
    boxCommandS = renderText "seta pra baixo mover pra Baixo" "" (-420, 150, 0.1, 0.1)
    boxCommandR = renderText "R - reniciar Jogo" "" (-420, 100, 0.1, 0.1)
    boxCommandK = renderText "Z - Rotacionar anti-horario" "" (-420, 50, 0.1, 0.1)
    boxCommandL = renderText "X - Rotacionar horario" "" (-420, 1, 0.1, 0.1)
    boxCommandX = renderText "espaco - Jogar a peca pra baixo" "" (-420, -50, 0.1, 0.1)
    boxOver
      | loseGame state = renderText "Voce" "Perdeu" (150, 100, 0.2, 0.2)
      | winGame state = renderText "Voce" "Venceu" (150, 100, 0.2, 0.2)
      | otherwise = renderText "" "" (150, -350, 0.2, 0.2)