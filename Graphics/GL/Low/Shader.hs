-- | A shader program is composed of two cooperating parts: the vertex program
-- and the fragment program. The vertex program is executed once for each
-- vertex. The fragment program is executed once for each pixel covered by
-- a rasterized primitive (actually this is more complicated but close enough).
--
-- The inputs to the vertex program are:
--
-- - a vertex (see "Graphics.GL.Low.VAO")
-- - uniforms
--
-- The outputs of the vertex program are:
--
-- - clip space position of the vertex, gl_Position
-- - any number of variables matching inputs to the fragment program
-- - (if rendering a point, then you can set gl_PointSize)
--
-- The inputs to the fragment program are:
--
-- - the previously mentioned outputs of the vertex program (interpolated)
-- - the window position of the pixel, gl_FragCoord
-- - samplers (see "Graphics.GL.Low.Texture")
-- - uniforms
-- - gl_FrontFacing, true if pixel is part of a front facing triangle
-- - (if rendering a point, then you can use gl_PointCoord)
--
-- The outputs of the fragment program are:
--
-- - a color (this is more complicated in reality but close enough)
-- - the depth of the pixel, gl_FragDepth, which will default to the pixel's Z.
--
-- = Example
--
-- @
-- module Main where
-- 
-- import Control.Monad.Loops (whileM_)
-- import Data.Functor ((\<$\>))
-- import qualified Data.Vector.Storable as V
-- import Data.Maybe (fromJust)
-- 
-- import qualified Graphics.UI.GLFW as GLFW
-- import Linear
-- import Graphics.GL.Low
-- 
-- main = do
--   GLFW.init
--   GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
--   GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
--   GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
--   GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
--   mwin <- GLFW.createWindow 640 480 \"Shaders\" Nothing Nothing
--   case mwin of
--     Nothing  -> putStrLn "createWindow failed"
--     Just win -> do
--       GLFW.makeContextCurrent (Just win)
--       GLFW.swapInterval 1
--       (vao, prog1, prog2, prog3) <- setup
--       whileM_ (not \<$\> GLFW.windowShouldClose win) $ do
--         GLFW.pollEvents
--         t <- (realToFrac . fromJust) \<$\> GLFW.getTime
--         draw vao prog1 prog2 prog3 t
--         GLFW.swapBuffers win
-- 
-- setup = do
--   vao <- newVAO
--   bindVAO vao
--   vsource <- readFile "shader.vert"
--   fsource1 <- readFile "shader1.frag"
--   fsource2 <- readFile "shader2.frag"
--   fsource3 <- readFile "shader3.frag"
--   prog1 <- newProgram vsource fsource1
--   prog2 <- newProgram vsource fsource2
--   prog3 <- newProgram vsource fsource3
--   useProgram prog1
--   let blob = V.fromList
--         [ -0.4, -0.4, 0, 0
--         ,  0,    0.4, 0, 1
--         ,  0.4, -0.4, 1, 1] :: V.Vector Float
--   vbo <- newVBO blob StaticDraw
--   bindVBO vbo
--   setVertexAttributeLayout
--     [ Attrib "position" 2 VFloat
--     , Attrib "location" 2 VFloat ]
--   return (vao, prog1, prog2, prog3)
-- 
-- draw vao prog1 prog2 prog3 t = do
--   clearColorBuffer (0,0,0)
--   bindVAO vao
--   drawThing prog1 t (V3 (-0.5)   0.5    0.0)
--   drawThing prog2 t (V3   0.5    0.5    0.0)
--   drawThing prog3 t (V3   0.0  (-0.5) (-0.0))
-- 
-- drawThing :: Program -> Float -> V3 Float -> IO ()
-- drawThing prog t shift = do
--   let angle = t / 5
--   let move = mkTransformation (axisAngle (V3 0 0 1) angle) shift
--   useProgram prog
--   setUniform1f "time" [t]
--   setUniform44 "move" [transpose move]
--   drawTriangles 3
-- @
--
-- Where the vertex shader is
--
-- @
-- #version 150
-- uniform mat4 move;
-- in vec2 position;
-- in vec2 location;
-- out vec2 Location;
-- void main()
-- {
--     gl_Position = move * vec4(position, 0.0, 1.0);
--     Location = location;
-- }
-- @
--
-- And the three fragment shaders are
--
-- @
-- #version 150
-- uniform float time;
-- in vec2 Location;
-- out vec4 outColor;
-- void main()
-- {
--   float x = gl_FragCoord.x / 640;
--   float y = gl_FragCoord.y / 480;
--   outColor = vec4(
--     fract(x*25) < 0.5 ? 1.0 : 0.0,
--     fract(y*25) < 0.5 ? 1.0 : 0.0,
--     0.0, 1.0
--   );
-- }
-- @
--
-- @
-- #version 150
-- uniform float time;
-- in vec2 Location;
-- out vec4 outColor;
-- void main()
-- {
--   outColor = vec4(
--     fract(Location.x*10) < 0.5 ? 1.0 : 0.0,
--     fract(Location.y*10) < 0.5 ? 1.0 : 0.0,
--     0.0, 1.0
--   );
-- }
-- @
--
-- @
-- #version 150
-- uniform float time;
-- in vec2 Location;
-- out vec4 outColor;
-- void main()
-- {
--   float t = time;
--   outColor = vec4(
--     fract(Location.x*5) < 0.5 ? sin(t*3.145) : cos(t*4.567),
--     fract(Location.y*5) < 0.5 ? cos(t*6.534) : sin(t*4.321),
--     0.0, 1.0
--   );
-- }
-- @
--
-- The output should look like
--
-- <<shaders.gif 3 Different Shaders Animated Demo>>
--
-- Where the window coordinates, the interpolated location on the triangle,
-- and the elapsed time are used to color the triangle respectively.
--

{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Low.Shader (
  Program,
  ProgramError(..),
  newProgramSafe,
  deleteProgram,
  newProgram,
  useProgram,
  setUniform1f,
  setUniform2f,
  setUniform3f,
  setUniform4f,
  setUniform1i,
  setUniform2i,
  setUniform3i,
  setUniform4i,
  setUniform44,
  setUniform33,
  setUniform22
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Exception
import Control.Monad (when, forM_)
import Data.Typeable

import Graphics.GL
import Linear

import Graphics.GL.Low.Classes
import Graphics.GL.Low.VertexAttrib

-- | Handle to a shader program.
newtype Program = Program GLuint deriving Show

-- | Either a vertex shader or a fragment shader.
data ShaderType = VertexShader | FragmentShader deriving Show

instance ToGL ShaderType where
  toGL VertexShader = GL_VERTEX_SHADER
  toGL FragmentShader = GL_FRAGMENT_SHADER

-- | The error message emitted by the driver when shader compilation or
-- linkage fails.
data ProgramError =
  VertexShaderError String |
  FragmentShaderError String |
  LinkError String
    deriving (Show, Typeable)
  
instance Exception ProgramError

-- | Same as 'newProgram' but does not throw exceptions.
newProgramSafe :: String -> String -> IO (Either ProgramError Program)
newProgramSafe vcode fcode = try $ newProgram vcode fcode

-- | Delete a program.
deleteProgram :: Program -> IO ()
deleteProgram (Program n) = glDeleteProgram n

-- | Compile the code for a vertex shader and a fragment shader, then link
-- them into a new program. If the compiler or linker fails it will throw
-- a ProgramError.
newProgram :: String -- ^ vertex shader source code
           -> String -- ^ fragment shader source code
           -> IO Program
newProgram vcode fcode = do
  vertexShaderId <- compileShader vcode VertexShader
  fragmentShaderId <- compileShader fcode FragmentShader
  programId <- glCreateProgram
  glAttachShader programId vertexShaderId
  glAttachShader programId fragmentShaderId
  glLinkProgram programId
  result <- alloca $ \ptr ->
    glGetProgramiv programId GL_LINK_STATUS ptr >> peek ptr
  when (result == GL_FALSE) $ do
    len <- fmap fromIntegral $ alloca $ \ptr ->
      glGetProgramiv programId GL_INFO_LOG_LENGTH ptr >> peek ptr
    errors <- allocaArray len $ \ptr -> do
      glGetProgramInfoLog programId (fromIntegral len) nullPtr ptr
      peekCString ptr
    throwIO (LinkError errors)
  glDeleteShader vertexShaderId
  glDeleteShader fragmentShaderId
  return (Program programId)

-- | Install a program into the rendering pipeline. Replaces the program
-- already in use, if any.
useProgram :: Program -> IO ()
useProgram (Program n) = glUseProgram n

compileShader :: String -> ShaderType -> IO GLuint
compileShader code vertOrFrag = do
  shaderId <- glCreateShader (toGL vertOrFrag)
  withCString code $ \ptr -> with ptr $ \pptr -> do
    glShaderSource shaderId 1 pptr nullPtr
    glCompileShader shaderId
  result <- with GL_FALSE $ \ptr ->
    glGetShaderiv shaderId GL_COMPILE_STATUS ptr >> peek ptr
  when (result == GL_FALSE) $ do
    len <- fmap fromIntegral $ alloca $ \ptr ->
      glGetShaderiv shaderId GL_INFO_LOG_LENGTH ptr >> peek ptr
    errors <- allocaArray len $ \ptr -> do
      glGetShaderInfoLog shaderId (fromIntegral len) nullPtr ptr
      peekCString ptr
    case vertOrFrag of
      VertexShader -> throwIO (VertexShaderError errors)
      FragmentShader -> throwIO (FragmentShaderError errors)
  return shaderId

setUniform1f :: String -> [Float] -> IO ()
setUniform1f = setUniform glUniform1fv

setUniform2f :: String -> [V2 Float] -> IO ()
setUniform2f = setUniform
  (\loc cnt val -> glUniform2fv loc cnt (castPtr val))

setUniform3f :: String -> [V3 Float] -> IO ()
setUniform3f = setUniform
  (\loc cnt val -> glUniform3fv loc cnt (castPtr val))

setUniform4f :: String -> [V4 Float] -> IO ()
setUniform4f = setUniform
  (\loc cnt val -> glUniform4fv loc cnt (castPtr val))

setUniform1i :: String -> [Int] -> IO ()
setUniform1i = setUniform
  (\loc cnt val -> glUniform1iv loc cnt (castPtr val))

setUniform2i :: String -> [V2 Int] -> IO ()
setUniform2i = setUniform 
  (\loc cnt val -> glUniform2iv loc cnt (castPtr val))

setUniform3i :: String -> [V3 Int] -> IO ()
setUniform3i = setUniform
  (\loc cnt val -> glUniform3iv loc cnt (castPtr val))

setUniform4i :: String -> [V4 Int] -> IO ()
setUniform4i = setUniform
  (\loc cnt val -> glUniform4iv loc cnt (castPtr val))

setUniform44 :: String -> [M44 Float] -> IO ()
setUniform44 = setUniform
  (\loc cnt val -> glUniformMatrix4fv loc cnt GL_FALSE (castPtr val))

setUniform33 :: String -> [M33 Float] -> IO ()
setUniform33 = setUniform
  (\loc cnt val -> glUniformMatrix3fv loc cnt GL_FALSE (castPtr val))

setUniform22 :: String -> [M22 Float] -> IO ()
setUniform22 = setUniform
  (\loc cnt val -> glUniformMatrix2fv loc cnt GL_FALSE (castPtr val))

setUniform :: Storable a
           => (GLint -> GLsizei -> Ptr a -> IO ())
           -> String
           -> [a]
           -> IO ()
setUniform glAction name xs = withArrayLen xs $ \n bytes -> do
  p <- alloca (\ptr -> glGetIntegerv GL_CURRENT_PROGRAM ptr >> peek ptr)
  if p == 0
    then return ()
    else do
      loc <- withCString name (\ptr -> glGetUniformLocation (fromIntegral p) ptr)
      if loc == -1
        then return ()
        else glAction loc (fromIntegral n) bytes


