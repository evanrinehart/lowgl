module Main where

import Control.Monad.Loops (whileM_)
import qualified Data.Vector.Storable as V
import Data.Maybe (fromJust)
import Data.Word

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Graphics.GL.Low

main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  mwin <- GLFW.createWindow 640 480 "Framebuffer" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      (vao1, vao2, prog1, prog2, fbo, texture) <- setup
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        t <- (realToFrac . fromJust) <$> GLFW.getTime
        draw vao1 vao2 prog1 prog2 fbo texture t
        GLFW.swapBuffers win

setup = do
  -- primary subject
  vao1 <- newVAO
  bindVAO vao1
  let blob = V.fromList
        [ -0.5, -0.5, 0, 0
        ,  0,    0.5, 0, 1
        ,  0.5, -0.5, 1, 1] :: V.Vector Float
  vbo1 <- newBufferObject blob StaticDraw
  bindVBO vbo1
  vsource  <- readFile "framebuffer.vert"
  fsource1 <- readFile "framebuffer1.frag"
  prog1 <- newProgram vsource fsource1
  useProgram prog1
  setVertexLayout
    [ Attrib "position" 2 GLFloat
    , Attrib "texcoord" 2 GLFloat ]

  -- full-screen quad to show the post-processed scene
  vao2 <- newVAO
  bindVAO vao2
  let blob = V.fromList
        [ -1, -1, 0, 0
        , -1,  1, 0, 1
        ,  1, -1, 1, 0
        ,  1,  1, 1, 1] :: V.Vector Float
  vbo2 <- newBufferObject blob StaticDraw
  bindVBO vbo2
  setVertexLayout
    [ Attrib "position" 2 GLFloat
    , Attrib "texcoord" 2 GLFloat ]
  indices <- newBufferObject (V.fromList [0,1,2,3,2,1] :: V.Vector Word8) StaticDraw
  bindElementArray indices
  fsource2 <- readFile "framebuffer2.frag"
  prog2 <- newProgram vsource fsource2
  useProgram prog2

  -- create an FBO to render the primary scene on
  fbo <- newFBO
  bindFBO fbo
  texture <- newEmptyTexture2D 640 480 RGB
  bindTexture2D texture
  setTex2DFiltering Linear
  attachTex2D texture
  return (vao1, vao2, prog1, prog2, fbo, texture)

draw :: VAO -> VAO -> Program -> Program -> FBO -> Texture -> Float -> IO ()
draw vao1 vao2 prog1 prog2 fbo texture t = do
  bindVAO vao1
  bindFBO fbo
  useProgram prog1
  clearColorBuffer (0,0,0)
  setUniform1f "time" [t]
  drawTriangles 3

  bindVAO vao2
  bindDefaultFramebuffer
  useProgram prog2
  bindTexture2D texture
  setUniform1f "time" [t]
  drawIndexedTriangles 6 UByteIndices
  
