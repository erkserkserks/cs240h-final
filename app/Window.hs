{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, 
             TemplateHaskell, TypeFamilies #-}
module Window where

import Control.Applicative
import Control.Lens
import Control.Monad
import GHC.Float
import Text.Printf (printf)

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Input

data WindowState = WindowState {
  _width :: Float,
  _height :: Float 
} deriving (Eq, Show)

processWindowState :: WindowState -> Float -> Float -> WindowState
processWindowState windowState width height = windowState'
  where windowState' = WindowState width height

