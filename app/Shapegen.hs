{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, 
             TemplateHaskell, TypeFamilies #-}
module Shapegen where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Fixed
import Text.Printf (printf)

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import "linear" Linear

data ShapegenState = ShapegenState {
  _shapes :: [(V4 Float, V3 Float)],
  _xStart :: Float,
  _zStart :: Float
} deriving (Eq, Show)
makeLenses ''ShapegenState


verticesPerShape = 3
maxShapes = 10000

initialShapegenState :: ShapegenState
initialShapegenState = ShapegenState [] 0 0


generateTriangle :: ShapegenState -> [(V4 Float, V3 Float)]
generateTriangle shapegenState = triangle
  where x = _xStart shapegenState
        z = _zStart shapegenState
        triangle =
                  [(V4 (-1+x) 1    (0+z) 1, V3 1 0 0),
                   (V4 (0+x)  (-1) (0+z) 1, V3 0 1 0),
                   (V4 (1+x)  1    (0+z) 1, V3 0 0 1)]

processShapegenState :: ShapegenState -> ShapegenState
processShapegenState shapegenState = shapegenState'
  where shapes = if ((length (_shapes shapegenState)) > maxShapes) -- Limit max number of shapes
                 then []
                 else _shapes shapegenState
        xStart' = (_xStart shapegenState + 0.02) `Data.Fixed.mod'` 5
        zStart' = (_zStart shapegenState + 0.02) `Data.Fixed.mod'` 7
        shapegenState' = ShapegenState ((generateTriangle shapegenState) ++ 
                                         shapes) xStart' zStart'

-- Create vertex data buffers
makeVertexBuffer :: ShapegenState -> ContextT GLFW.GLFWWindow os (ContextFormat
  RGBFloat Depth) IO (Buffer os (B4 Float, B3 Float))
makeVertexBuffer shapegenState = do
  let triangleVertices = _shapes shapegenState
      numTriangles = (length triangleVertices) `div` verticesPerShape
  vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer numTriangles
  writeBuffer vertexBuffer 0 triangleVertices
                            
  return vertexBuffer
                    
-- Make a Render action that returns a PrimitiveArray for the triangles
makePrimitives :: (Buffer os (B4 Float, B3 Float)) -> ShapegenState ->
  Render os (ContextFormat RGBFloat Depth) (PrimitiveArray Triangles (
  B4 Float, B3 Float))
makePrimitives vertexBuffer shapegenState = do
  vertexArray <- newVertexArray vertexBuffer
  return $ toPrimitiveArray TriangleList vertexArray

