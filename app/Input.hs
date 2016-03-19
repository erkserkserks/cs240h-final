{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, 
             TemplateHaskell, TypeFamilies #-}
module Input where

import Control.Applicative
import Control.Lens
import Control.Monad
import GHC.Float
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Graphics.GPipe
import Text.Printf (printf)

data InputState = InputState {
  _lastCursorX :: Float,
  _lastCursorY :: Float,
  _arcballOn :: Bool
} deriving (Eq, Show)

-- Keyboard and mouse input
data KbmInput = KbmInput {
  wKey :: Bool,
  aKey :: Bool,
  sKey :: Bool,
  dKey :: Bool,
  mouse1 :: Bool,
  mouse2 :: Bool,
  cursorX :: Float,
  cursorY :: Float
} deriving (Eq, Show)

initialInputState :: InputState
initialInputState = InputState 0 0 False

processInputState :: InputState -> KbmInput -> InputState
processInputState inputState input = inputState'
  where arcballOn' = if (mouse1 input) then True else False
        (lastCursorX', lastCursorY') = if (arcballOn')
                                       then (cursorX input, cursorY input)
                                       else (_lastCursorX inputState,
                                             _lastCursorY inputState)
        inputState' = InputState lastCursorX' lastCursorY' arcballOn'

-- Helper function to get Boolean representation of a keypress
keyStateToBool keyState =
  if keyState == GLFW.KeyState'Pressed then True else False

mouseButtonStateToBool mouseButton =
  if mouseButton == GLFW.MouseButtonState'Pressed then True else False

-- Get current keyboard and mouse input
getKbmInput :: ContextT GLFW.GLFWWindow os (ContextFormat RGBFloat Depth) IO KbmInput
getKbmInput = do
  wKey <- GLFW.getKey GLFW.Key'W
  aKey <- GLFW.getKey GLFW.Key'A
  sKey <- GLFW.getKey GLFW.Key'S
  dKey <- GLFW.getKey GLFW.Key'D
  mouse1 <- GLFW.getMouseButton GLFW.MouseButton'1
  mouse2 <- GLFW.getMouseButton GLFW.MouseButton'2
  (cursorX, cursorY) <- GLFW.getCursorPos

  return $ KbmInput {
    wKey = keyStateToBool wKey,
    aKey = keyStateToBool aKey,
    sKey = keyStateToBool sKey,
    dKey = keyStateToBool dKey,
    mouse1 = mouseButtonStateToBool mouse1,
    mouse2 = mouseButtonStateToBool mouse2,
    cursorX = double2Float cursorX,
    cursorY = double2Float cursorY
  }
