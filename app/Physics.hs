{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, 
             TemplateHaskell, TypeFamilies #-}
module Physics where

import Control.Applicative
import Control.Lens
import Control.Monad
import Text.Printf (printf)

import "linear" Linear

import Shapegen

-- TODO(ethong): Add a triangle type
data PhysicsState = PhysicsState {
  _velocities :: [Double]
} deriving (Eq, Show)

initialPhysicsState :: PhysicsState
initialPhysicsState = PhysicsState []

applyVelocity :: (V4 Float, V3 Float) -> (V4 Float, V3 Float)
applyVelocity triangle = triangle'
  where y = triangle ^. _1 ^. _y -- Get current y
        y' = y - 0.02  -- Apply velocity
        triangle' = set (_1._y) y' triangle -- Get triangle with y'
        

processPhysicsState :: PhysicsState -> ShapegenState -> (PhysicsState, ShapegenState)
processPhysicsState physicsState shapegenState = (physicsState', shapegenState')
  where physicsState' = physicsState
        shapes'  = fmap applyVelocity (_shapes shapegenState)
        shapegenState' = set shapes shapes' shapegenState

