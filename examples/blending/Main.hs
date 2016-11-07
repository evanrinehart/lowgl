{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Loops (whileM_)
import Data.Functor ((<$>))
import qualified Data.Vector.Storable as V
import Control.Concurrent.STM

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Graphics.GL.Low

main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  mwin <- GLFW.createWindow 640 480 "Blending" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      shouldSwap <- newTVarIO False
      (GLFW.setKeyCallback win . Just)
        (\_ _ _ _ _ -> atomically (modifyTVar shouldSwap not))
      (vao, prog) <- setup
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        draw vao prog shouldSwap
        GLFW.swapBuffers win

setup = do
  vao <- newVAO
  bindVAO vao
  vsource <- readFile "blending.vert"
  fsource <- readFile "blending.frag"
  prog <- newProgram vsource fsource
  useProgram prog
  let blob = V.fromList
        [ -0.5,  0.5
        ,  0.5,    0
        , -0.5, -0.5 ] :: V.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  setVertexLayout [Attrib "position" 2 (GLScalarAttrib (GLFloat Single))]
  enableBlending basicBlending
  return (vao, prog)

draw vao prog shouldSwap = do
  clearColorBuffer (0,0,0)
  yes <- readTVarIO shouldSwap
  if yes
    then sequence [drawRed, drawGreen]
    else sequence [drawGreen, drawRed]

drawGreen = do
  setUniform3f "color" [V3 0 1 0]
  setUniform1f "alpha" [0.5]
  setUniform44 "move" [eye4]
  drawTriangles 3

drawRed = do
  let ninety = pi/2
  let move = mkTransformation (axisAngle (V3 0 0 1) ninety) (V3 0.25 0.5 0)
  setUniform3f "color" [V3 1 0 0]
  setUniform1f "alpha" [0.5]
  setUniform44 "move" [transpose move]
  drawTriangles 3
