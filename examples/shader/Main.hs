module Main where

import Control.Monad.Loops (whileM_)
import qualified Data.Vector.Storable as V
import Data.Maybe (fromJust)

import qualified Graphics.UI.GLFW as GLFW
import Linear

import Graphics.GL.Low

main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  mwin <- GLFW.createWindow 640 480 "Shaders" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      (vao, prog1, prog2, prog3) <- setup
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        t <- (realToFrac . fromJust) <$> GLFW.getTime
        draw vao prog1 prog2 prog3 t
        GLFW.swapBuffers win

setup = do
  vao <- newVAO
  bindVAO vao
  vsource <- readFile "shader.vert"
  fsource1 <- readFile "shader1.frag"
  fsource2 <- readFile "shader2.frag"
  fsource3 <- readFile "shader3.frag"
  prog1 <- newProgram vsource fsource1
  prog2 <- newProgram vsource fsource2
  prog3 <- newProgram vsource fsource3
  useProgram prog1
  let blob = V.fromList
        [ -0.4, -0.4, 0, 0
        ,  0,    0.4, 0, 1
        ,  0.4, -0.4, 1, 1] :: V.Vector Float
  vbo <- newBufferObject blob StaticDraw
  bindVBO vbo
  setVertexLayout
    [ Attrib "position" 2 GLFloat
    , Attrib "location" 2 GLFloat ]
  return (vao, prog1, prog2, prog3)

draw vao prog1 prog2 prog3 t = do
  clearColorBuffer (0,0,0)
  bindVAO vao
  drawThing prog1 t (V3 (-0.5)   0.5    0.0)
  drawThing prog2 t (V3   0.5    0.5    0.0)
  drawThing prog3 t (V3   0.0  (-0.5) (-0.0))

drawThing :: Program -> Float -> V3 Float -> IO ()
drawThing prog t shift = do
  let angle = t / 5
  let move = mkTransformation (axisAngle (V3 0 0 1) angle) shift
  useProgram prog
  setUniform1f "time" [t]
  setUniform44 "move" [transpose move]
  drawTriangles 3
