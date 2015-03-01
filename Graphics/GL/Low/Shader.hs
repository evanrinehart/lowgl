
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Low.Shader (
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
  newProgram, newProgramSafe, makeProgram, loadProgram, deleteProgram,
  newShader, newShaderSafe, loadShader, loadShaderType, guessShaderFileType,
  
  activeAttribs, activeUniforms,
  
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
  setUniform22,
  Program,
  ProgramError(..)

  -- * Example
  -- $example
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Exception
import Control.Applicative
import Control.Monad (when, forM_)
import Data.Tuple (swap)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.State (state, runState)
import System.FilePath

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Loops

import Graphics.GL
import Linear

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Internal.Shader
import Graphics.GL.Low.Internal.Common
import Graphics.GL.Low.Classes
import Graphics.GL.Low.VertexAttrib


-- | Same as 'newProgram' but does not throw exceptions.
newProgramSafe :: (MonadIO m) => String -> String -> m (Either ProgramError Program)
newProgramSafe vcode fcode = liftIO . try $ newProgram vcode fcode

newShaderSafe :: (MonadIO m) => String -> ShaderType -> m (Either ProgramError Shader)
newShaderSafe src ty = liftIO . try $ newShader src ty


-- | Compile the code for a vertex shader and a fragment shader, then link
-- them into a new program. If the compiler or linker fails it will throw
-- a ProgramError.
newProgram :: (MonadIO m) 
           => String -- ^ vertex shader source code
           -> String -- ^ fragment shader source code
           -> m Program
newProgram vcode fcode = do
  vertexShader <- newShader vcode VertexShader
  fragmentShader <- newShader fcode FragmentShader
  makeProgram [vertexShader, fragmentShader]

newShader :: (MonadIO m) => String -> ShaderType -> m Shader
newShader code vertOrFrag = do
  shader <- createShader vertOrFrag
  shaderSource shader (T.pack code)
  compileShader shader
  compiled <- compileStatus shader
  when (not compiled) $ do
    errors <- shaderInfoLog shader
    case vertOrFrag of
      VertexShader -> throwM $ VertexShaderError errors
      FragmentShader -> throwM $ FragmentShaderError errors
  return shader

-- | Compile a collection of shaders into a program, then delete the shader objects.
--   Throws exceptions on compile/link failure.
makeProgram :: (MonadIO m) => [Shader] -> m Program
makeProgram shaders = do
    program <- createProgram
    mapM_ (attachShader program) shaders
    linkProgram program
    linked <- linkStatus program
    when (not linked) $ do
        errors <- programInfoLog program
        throwM $ LinkError errors
    mapM_ (detachShader program) shaders
    mapM_ deleteShader shaders
    return program

loadShader :: (MonadIO m) => FilePath -> m Shader
loadShader path = case guessShaderFileType path of
    Nothing -> throwM . ShaderError Nothing $ "couldn't guess shader type from file name: " ++ path
    Just ty -> loadShaderType ty path

loadShaderType :: (MonadIO m) => ShaderType -> FilePath -> m Shader
loadShaderType ty path = do
    src <- liftIO $ readFile path
    newShader src ty

loadProgram :: (MonadIO m) => [FilePath] -> m Program
loadProgram paths = makeProgram =<< mapM loadShader paths

guessShaderFileType :: FilePath -> Maybe ShaderType
guessShaderFileType path 
    | any (check "v") exts'   = Just VertexShader
    | any (check "f") exts'   = Just FragmentShader
    | "-vs" `isSuffixOf` base = Just VertexShader
    | "-fs" `isSuffixOf` base = Just FragmentShader
    | otherwise               = Nothing
  where (exts, base) = runState (unfoldWhileM (not . null) (state $ swap . splitExtension)) path
        exts' = dropWhile (== '.') <$> exts
        check x ext = x `isPrefixOf` ext || x `isSuffixOf` ext


activeAttribs :: (MonadIO m) => Program -> m [ShaderAttrib]
activeAttribs p = do
    aMax <- activeAttributeCount p
    mapM (getShaderAttrib p) [0..aMax - 1]

activeUniforms :: (MonadIO m) => Program -> m [ShaderUniform]
activeUniforms p = do
    uMax <- activeUniformCount p
    mapM (getShaderUniform p) [0..uMax - 1]
    

programInfoLog :: (MonadIO m) => Program -> m String
programInfoLog p = do
    len <- programInfoLogLength p
    liftIO . allocaArray len $ 
        \ptr -> do
            glGetProgramInfoLog (fromProgram p) (fromIntegral len) nullPtr ptr
            peekCString ptr

shaderInfoLog :: (MonadIO m) => Shader -> m String
shaderInfoLog s = do
    len <- shaderInfoLogLength s
    liftIO . allocaArray len $ 
        \ptr -> do
            glGetShaderInfoLog (fromShader s) (fromIntegral len) nullPtr ptr
            peekCString ptr


-- | Install a program into the rendering pipeline. Replaces the program
-- already in use, if any.
useProgram :: (MonadIO m) => Program -> m ()
useProgram (Program n) = glUseProgram n

setUniform1f :: (MonadIO m) => Text -> [Float] -> m ()
setUniform1f = setUniform glUniform1fv

setUniform2f :: (MonadIO m) => Text -> [V2 Float] -> m ()
setUniform2f = setUniform
  (\loc cnt val -> glUniform2fv loc cnt (castPtr val))

setUniform3f :: (MonadIO m) => Text -> [V3 Float] -> m ()
setUniform3f = setUniform
  (\loc cnt val -> glUniform3fv loc cnt (castPtr val))

setUniform4f :: (MonadIO m) => Text -> [V4 Float] -> m ()
setUniform4f = setUniform
  (\loc cnt val -> glUniform4fv loc cnt (castPtr val))

setUniform1i :: (MonadIO m) => Text -> [Int] -> m ()
setUniform1i = setUniform
  (\loc cnt val -> glUniform1iv loc cnt (castPtr val))

setUniform2i :: (MonadIO m) => Text -> [V2 Int] -> m ()
setUniform2i = setUniform 
  (\loc cnt val -> glUniform2iv loc cnt (castPtr val))

setUniform3i :: (MonadIO m) => Text -> [V3 Int] -> m ()
setUniform3i = setUniform
  (\loc cnt val -> glUniform3iv loc cnt (castPtr val))

setUniform4i :: (MonadIO m) => Text -> [V4 Int] -> m ()
setUniform4i = setUniform
  (\loc cnt val -> glUniform4iv loc cnt (castPtr val))

setUniform44 :: (MonadIO m) => Text -> [M44 Float] -> m ()
setUniform44 = setUniform
  (\loc cnt val -> glUniformMatrix4fv loc cnt GL_FALSE (castPtr val))

setUniform33 :: (MonadIO m) => Text -> [M33 Float] -> m ()
setUniform33 = setUniform
  (\loc cnt val -> glUniformMatrix3fv loc cnt GL_FALSE (castPtr val))

setUniform22 :: (MonadIO m) => Text -> [M22 Float] -> m ()
setUniform22 = setUniform
  (\loc cnt val -> glUniformMatrix2fv loc cnt GL_FALSE (castPtr val))


-- $example
--
-- <<shaders.gif 3 Different Shaders Animated Demo>>
--
-- This example renders three differently-shaded triangles. The window
-- coordinates, the interpolated location on the triangle, and the elapsed time
-- are used to color the triangles respectively.
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
--   setVertexLayout
--     [ Attrib "position" 2 GLFloat
--     , Attrib "location" 2 GLFloat ]
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
