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

-- | A Program object is the combination of a compiled vertex shader and fragment
-- shader. Programs have three kinds of inputs: vertex attributes, uniforms,
-- and samplers. Programs have two outputs: fragment color and fragment depth.
-- At most one program can be "in use" at a time. Same idea as binding targets
-- it's just not called that.
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


