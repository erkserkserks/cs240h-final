{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, 
             TemplateHaskell, TypeFamilies #-}
module Main where

import Control.Applicative
import Control.Arrow (first)  
import Control.Lens
import Control.Monad
import Text.Printf (printf)

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import "linear" Linear

import Camera
import Input
import Physics
import Shapegen
import Window


main = do
  -- Set up the GLFW window
  let windowWidth = 800
      windowHeight = 600 
      windowConf = GLFW.WindowConf windowWidth windowHeight "Cool Demo"
      context = GLFW.newContext' [] windowConf

  runContextT context (ContextFormatColorDepth SRGB8 Depth16) $ do
    -- Create a buffer for the uniform values
    uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1

    -- Create the shader
    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream primitives
      (modelViewProj, normMat) <- getUniform (const (uniform, 0))
      let transformedPrims = fmap (first (applyTransform modelViewProj)) primitiveStream
      fragmentStream <- rasterize rasterOptions transformedPrims
      let depthFrags = withRasterizedInfo
              (\a x -> (a, (rasterizedFragCoord x) ^. _z)) fragmentStream
      let colorOption = ContextColorOption NoBlending (pure True)
          depthOption = DepthOption Less True

      drawContextColorDepth (const (colorOption, depthOption)) depthFrags

    -- Run the loop
    let inputState = Input.initialInputState
        cameraState = Camera.initialCameraState
        windowState = WindowState (fromIntegral windowWidth)
                                  (fromIntegral windowHeight)
        shapegenState = Shapegen.initialShapegenState
        physicsState = Physics.initialPhysicsState
    loop shader uniform inputState cameraState windowState shapegenState
      physicsState


loop :: CompiledShader os (ContextFormat RGBFloat Depth) ShaderEnvironment 
     -> Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float)))
     -> InputState
     -> CameraState
     -> WindowState
     -> ShapegenState
     -> PhysicsState
     -> ContextT GLFW.GLFWWindow os (ContextFormat RGBFloat Depth) IO ()

loop shader uniform inputState cameraState windowState shapegenState physicsState = do
  shouldClose <- GLFW.windowShouldClose

  input <- Input.getKbmInput
  size@(V2 w h) <- getContextBuffersSize

  let inputState' = Input.processInputState inputState input
      windowState' = Window.processWindowState windowState (fromIntegral w)
                     (fromIntegral h)
      cameraState' = Camera.processCameraState cameraState windowState
                     windowState' inputState inputState'
      shapegenStateTemp = Shapegen.processShapegenState shapegenState 
      (physicsState', shapegenState') = Physics.processPhysicsState
                                        physicsState shapegenStateTemp

  let modelRot = axisAngle (V3 0.2 0.2 0.2) 0
      modelMat = mkTransformationMat (fromQuaternion modelRot) (pure 0)
      viewRot = _rot cameraState'
      viewTrans = _trans cameraState'
      viewMat = mkTransformationMat (fromQuaternion viewRot) viewTrans
      projMat = perspective (75 * pi / 180) (fromIntegral w / fromIntegral h) 1 100
      mvpMat = projMat !*! viewMat !*! modelMat
      normMat = fromQuaternion modelRot
  writeBuffer uniform 0 [(mvpMat, normMat)]
  vertexBuffer <- makeVertexBuffer shapegenState'
  -- Render the frame and present the results
  render $ do
    clearContextColor 0 -- Black
    clearContextDepth 1 -- Far plane
    prims <- Shapegen.makePrimitives vertexBuffer shapegenState'
    shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapContextBuffers

  closeRequested <- GLFW.windowShouldClose
  unless closeRequested $
    loop shader uniform inputState' cameraState' windowState' shapegenState'
      physicsState'

data ShaderEnvironment = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles (B4 Float, B3 Float)
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

applyTransform modelViewProj vertex =
  modelViewProj !* vertex


