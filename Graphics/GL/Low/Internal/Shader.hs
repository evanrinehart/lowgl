module Graphics.GL.Low.Internal.Shader where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Exception
import Control.Monad (liftM, when, forM_)
import Control.Monad.IO.Class

import Graphics.GL

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes

createProgram :: (MonadIO m) => m Program
createProgram = liftM Program glCreateProgram

linkProgram :: (MonadIO m) => Program -> m ()
linkProgram p = glLinkProgram (fromProgram p)

deleteShader :: (MonadIO m) => Shader -> m ()
deleteShader s = glDeleteShader (fromShader s)

attachShader :: (MonadIO m) => Program -> Shader -> m ()
attachShader p s = glAttachShader (fromProgram p) (fromShader s)

linkStatus :: (MonadIO m) => Program -> m Bool
linkStatus p = liftM (fromGLWith False) $ getProgramiv p GL_LINK_STATUS

infoLogLength :: (MonadIO m, Num a) => Program -> m a
infoLogLength p = getProgramiv p GL_INFO_LOG_LENGTH

activeAttributeCount :: (MonadIO m) => Program -> m AttribLocation
activeAttributeCount p = AttribLocation `liftM` getProgramiv p GL_ACTIVE_ATTRIBUTES

activeAttributeMaxLength :: (MonadIO m) => Program -> m GLint
activeAttributeMaxLength p = getProgramiv p GL_ACTIVE_ATTRIBUTE_MAX_LENGTH

activeUniformCount :: (MonadIO m) => Program -> m UniformLocation
activeUniformCount p = UniformLocation `liftM` getProgramiv p GL_ACTIVE_UNIFORMS

activeUniformMaxLength :: (MonadIO m) => Program -> m GLint
activeUniformMaxLength p = getProgramiv p GL_ACTIVE_UNIFORM_MAX_LENGTH


getProgramiv :: (MonadIO m, Num a) => Program -> GLenum -> m a
getProgramiv p q = liftIO . alloca $ 
    \ptr -> glGetProgramiv (fromProgram p) q ptr >> liftM fromIntegral (peek ptr)



compileShader :: (MonadIO m) => String -> ShaderType -> m Shader
compileShader code vertOrFrag = liftIO $ do
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
  return $ Shader shaderId


setUniform :: (MonadIO m, Storable a)
           => (GLint -> GLsizei -> Ptr a -> IO ())
           -> String
           -> [a]
           -> m ()
setUniform glAction name xs = liftIO . withArrayLen xs $ \n bytes -> do
  p <- alloca (\ptr -> glGetIntegerv GL_CURRENT_PROGRAM ptr >> peek ptr)
  if p == 0
    then return ()
    else do
      loc <- withCString name (\ptr -> glGetUniformLocation (fromIntegral p) ptr)
      if loc == -1
        then return ()
        else glAction loc (fromIntegral n) bytes

