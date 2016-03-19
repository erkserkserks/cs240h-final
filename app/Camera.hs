{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, 
             TemplateHaskell, TypeFamilies #-}
module Camera where

import Control.Applicative
import Control.Lens
import Control.Monad
import GHC.Float
import Text.Printf (printf)

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import "linear" Linear

import Input
import Window

data CameraState = CameraState {
  _rot :: Quaternion Float,
  _trans :: V3 Float 
} deriving (Eq, Show)

initialCameraState :: CameraState
initialCameraState = CameraState (axisAngle (V3 1 0 0) 0) (-V3 0 0 10)

-- Implements an arcball camera
processCameraState :: CameraState -> WindowState -> WindowState ->
                      InputState -> InputState -> CameraState
processCameraState cameraState prevWindowState currWindowState prevInputState
                   currInputState = cameraState'
  where
    rot = _rot cameraState

    currArcballOn = _arcballOn currInputState
    prevArcballOn = _arcballOn prevInputState
    currLastCursorX = _lastCursorX currInputState
    currLastCursorY = _lastCursorY currInputState
    prevLastCursorX = _lastCursorX prevInputState
    prevLastCursorY = _lastCursorY prevInputState

    rot' = if (not currArcballOn || (currArcballOn && not prevArcballOn))
           then rot --Use existing rotation
           else arcballRot * rot -- Rotate existing rotation by arcball rotation
            where
              vector1 = getArcballVector prevLastCursorX prevLastCursorY
                        (_width prevWindowState) (_height prevWindowState)
              vector2 = getArcballVector currLastCursorX currLastCursorY
                        (_width currWindowState) (_height currWindowState)
              arcballRot = getRotation vector1 vector2

    trans' = _trans cameraState
    cameraState' = CameraState rot' trans'

-- Get normalized vector from the origin to point p on an virtual sphere.
-- P is aligned with the screen's (x, y) coordinates.
getArcballVector :: Float -> Float -> Float -> Float -> V3 Float
getArcballVector x y windowWidth windowHeight = a'
  where a = V3 (x / windowWidth * 2 - 1.0)
                (1.0 - (y / windowHeight * 2))
                0
        a_squared = (a ^. _x) * (a ^. _x) + (a ^. _y) * (a ^. _y)
        a' = if a_squared > 1
            then
              normalize a -- Get nearest point on sphere
            else
              a & _z .~ sqrt(1 - a_squared) -- Get point inside sphere

-- Get rotation between two vectors
getRotation :: V3 Float -> V3 Float -> Quaternion Float
getRotation v1 v2 = axisAngle rotationAxis angle
  where rotationAxis = v1 `cross` v2
        angle = acos $ min 1.0 (v1 `dot` v2)

