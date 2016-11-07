module Main where

import Control.Monad.Loops (whileM_)
import Data.Functor ((<$>))
import qualified Data.Vector.Storable as V

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Low

-- GLFW will be the shell of the demo
main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  mwin <- GLFW.createWindow 640 480 "Hello World" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      (vao, prog) <- setup -- load and configure objects
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        draw vao prog -- render
        GLFW.swapBuffers win

setup = do
  -- establish a VAO
  vao <- newVAO
  bindVAO vao
  -- load shader program
  vsource <- readFile "hello.vert"
  fsource <- readFile "hello.frag"
  prog <- newProgram vsource fsource
  useProgram prog
  -- load vertex data: three 2D vertex positions
  let blob = V.fromList
        [ -0.5, -0.5
        ,    0,  0.5
        ,  0.5, -0.5 ] :: V.Vector Float
  vbo <- newBufferObject blob StaticDraw
  bindVBO vbo
  -- connect program to vertex data via the VAO
  setVertexLayout [Attrib "position" 2 GLFloat]
  return (vao, prog)

draw vao prog = do
  clearColorBuffer (0,0,0)
  bindVAO vao
  useProgram prog
  drawTriangles 3
