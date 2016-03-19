{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
module Main where

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)

import System.Random

-- Generate random points in 2d
getRandomPoints2D :: (Float, Float) -> StdGen -> [V4 Float]
getRandomPoints2D range sg = zipWith createPoint x y
    where createPoint x y = V4 x y 0 1
          (sg1, sg2) = split sg
          x = randomRs range sg1
          y = randomRs range sg2

getPoints :: Int -> StdGen -> [V4 Float]
getPoints n sg = take n $ getRandomPoints2D (-1,1) sg

examplePoints :: Int -> IO [V4 Float]
examplePoints n = newStdGen >>= return . getPoints n

-- Quads from point
quadFromPoint :: V4 Float -> [V4 Float]
quadFromPoint p = map (p +) [ v4 (-off) (-off), v4 (-off) off
                            , v4 off (-off), v4 off (-off)
                            , v4 off off, v4 (-off) off
                            ]
    where off = 0.1
          v4 x y = V4 x y 0 0

main = do
    let width = 720
        height = 540
        windowConf = GLFW.WindowConf width height "hs-visu"
        context = GLFW.newContext' [] windowConf
    let numPoints = 6
    points <- examplePoints numPoints
    let quadPoints = concatMap quadFromPoint points
        pointsWithColor = zip quadPoints $ repeat (V3 1 0 0)
    runContextT context (ContextFormatColor RGB8) $ do

        -- Create buffers for the vertices
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (length pointsWithColor)
        writeBuffer vertexBuffer 0 pointsWithColor

        -- Create a buffer for the uniform values
        uniformBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1

        shader <- compileShader $ do
            -- Vertex shader
            primitiveStream <- toPrimitiveStream primitives
            modelViewProj <- getUniform (const (uniformBuffer, 0))
            let projPrimitiveStream = proj modelViewProj <$> primitiveStream

            -- Fragment shader
            fragmentStream <- rasterize rasterOptions projPrimitiveStream
            let colorOption = ContextColorOption NoBlending (pure True)
            drawContextColor (const colorOption) fragmentStream

        -- Run the loop
        loop vertexBuffer shader uniformBuffer

loop :: Buffer os (B4 Float, B3 Float) 
     -> CompiledShader os (ContextFormat RGBFloat ()) ShaderEnvironment 
     -> Buffer os (Uniform (V4 (B4 Float)))
     -> ContextT GLFW.GLFWWindow os (ContextFormat RGBFloat ()) IO ()

loop vertexBuffer shader uniformBuffer = do
    -- Construct the ModelViewProjection matrix
    size@(V2 w h) <- getContextBuffersSize
    let halfWidth = (fromIntegral w)/2.0
        halfHeight = (fromIntegral h)/2.0
        projMat = ortho (-halfWidth) halfWidth halfHeight (-halfHeight) -1.0 1.0

    writeBuffer uniformBuffer 0 [projMat]

    render $ do
        clearContextColor 1 -- White background
        clearContextDepth 1 -- Far plane
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader $ ShaderEnvironment primitiveArray (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
    swapContextBuffers

    closeRequested <- GLFW.windowShouldClose
    unless closeRequested $ loop vertexBuffer shader uniformBuffer

data ShaderEnvironment = ShaderEnvironment
    { primitives :: PrimitiveArray Triangles (B4 Float, B3 Float)
    , rasterOptions :: (Side, ViewPort, DepthRange)
    }

-- Projects the position of the vertices with the ModelViewProjection matrix
proj :: (V4 (V4 VFloat)) -> (V4 VFloat, V3 VFloat) -> (V4 VFloat, V3 VFloat)
proj modelViewProj (p,c) = (modelViewProj !* p, c)
