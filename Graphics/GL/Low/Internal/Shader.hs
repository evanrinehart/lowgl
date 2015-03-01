module Graphics.GL.Low.Internal.Shader where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Exception
import Control.Monad (liftM, when, forM_)
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T

import Graphics.GL

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Internal.Common
import Graphics.GL.Low.Classes


setUniform :: (MonadIO m, Storable a)
           => (GLint -> GLsizei -> Ptr a -> IO ())
           -> Text
           -> [a]
           -> m ()
setUniform glAction name xs = liftIO . withArrayLen xs $ \n bytes -> do
  --  p <- alloca (\ptr -> glGetIntegerv GL_CURRENT_PROGRAM ptr >> peek ptr)
  p <- currentProgram
  case p of
    Nothing   -> return ()
    Just prog -> do
        --  loc <- T.withCStringLen name (\(ptr, _) -> glGetUniformLocation (fromProgram p) ptr)
        l <- getUniformLocation prog name
        case l of
            Nothing  -> return ()
            Just loc -> glAction (fromIntegral $ fromUniformLocation loc) (fromIntegral n) bytes


currentProgram :: (MonadIO m) => m (Maybe Program)
currentProgram = do
    p <- getIntegerv GL_CURRENT_PROGRAM
    return $ if p > 0 then Just $ Program p else Nothing

createProgram :: (MonadIO m) => m Program
createProgram = liftM Program glCreateProgram

-- | Delete a program.
deleteProgram :: (MonadIO m) => Program -> m ()
deleteProgram (Program n) = glDeleteProgram n

attachShader :: (MonadIO m) => Program -> Shader -> m ()
attachShader p s = glAttachShader (fromProgram p) (fromShader s)

detachShader :: (MonadIO m) => Program -> Shader -> m ()
detachShader p s = glDetachShader (fromProgram p) (fromShader s)

linkProgram :: (MonadIO m) => Program -> m ()
linkProgram p = glLinkProgram (fromProgram p)

linkStatus :: (MonadIO m) => Program -> m Bool
linkStatus p = liftM (fromGLWith False) $ getProgramiv p GL_LINK_STATUS

programInfoLogLength :: (MonadIO m, Num a) => Program -> m a
programInfoLogLength p = getProgramiv p GL_INFO_LOG_LENGTH



createShader :: (MonadIO m) => ShaderType -> m Shader
createShader = liftM Shader . glCreateShader . toGL

deleteShader :: (MonadIO m) => Shader -> m ()
deleteShader = glDeleteShader . fromShader

shaderSource :: (MonadIO m) => Shader -> Text -> m ()
shaderSource shader src = liftIO . T.withCStringLen src $ 
    \(ptr, _) -> with ptr $ 
        \pptr -> glShaderSource (fromShader shader) 1 pptr nullPtr

compileShader :: (MonadIO m) => Shader -> m ()
compileShader = glCompileShader . fromShader

compileStatus :: (MonadIO m) => Shader -> m Bool
compileStatus p = liftM (fromGLWith False) $ getShaderiv p GL_COMPILE_STATUS

shaderInfoLogLength :: (MonadIO m, Num a) => Shader -> m a
shaderInfoLogLength p = getShaderiv p GL_INFO_LOG_LENGTH





activeAttributeCount :: (MonadIO m) => Program -> m AttribIndex
activeAttributeCount p = AttribIndex `liftM` getProgramiv p GL_ACTIVE_ATTRIBUTES

activeAttributeMaxLength :: (MonadIO m) => Program -> m GLsizei
activeAttributeMaxLength p = getProgramiv p GL_ACTIVE_ATTRIBUTE_MAX_LENGTH

activeUniformCount :: (MonadIO m) => Program -> m UniformIndex
activeUniformCount p = UniformIndex `liftM` getProgramiv p GL_ACTIVE_UNIFORMS

activeUniformMaxLength :: (MonadIO m) => Program -> m GLsizei
activeUniformMaxLength p = getProgramiv p GL_ACTIVE_UNIFORM_MAX_LENGTH

getActiveAttrib :: (MonadIO m) => Program -> AttribIndex -> m (Text, GLAttribType, Int)
getActiveAttrib p i = do
    maxLen <- activeAttributeMaxLength p
    (n, ty, sz) <- getActive maxLen $ glGetActiveAttrib (fromProgram p) (fromAttribIndex i) maxLen
    case fromGL ty of
        Nothing     -> throwM . ShaderTypeError $ unwords ["Unknown GLSL type:", show ty, "for shader attribute", T.unpack n]
        Just attrTy -> return $ (n, attrTy, fromIntegral sz)

getShaderAttrib :: (MonadIO m) => Program -> AttribIndex -> m ShaderAttrib
getShaderAttrib p i = do
    (n, ty, sz) <- getActiveAttrib p i
    loc <- getAttribLocation p n
    loc' <- case loc of
                Nothing -> throwM . ShaderTypeError $ "Error loading attrib " ++ T.unpack n
                Just l  -> return l
    return $ ShaderVar n loc' ty sz

getActiveUniform :: (MonadIO m) => Program -> UniformIndex -> m (Text, GLUniformType, Int)
getActiveUniform p i = do
    maxLen <- activeUniformMaxLength p
    (n, ty, sz) <- getActive maxLen $ glGetActiveUniform (fromProgram p) (fromUniformIndex i) maxLen
    case fromGL ty of
        Nothing     -> throwM . ShaderTypeError $ unwords ["Unknown GLSL type:", show ty, "for shader uniform", T.unpack n]
        Just attrTy -> return $ (n, attrTy, fromIntegral sz)

getShaderUniform :: (MonadIO m) => Program -> UniformIndex -> m ShaderUniform
getShaderUniform p i = do
    (n, ty, sz) <- getActiveUniform p i
    loc <- getUniformLocation p n
    loc' <- case loc of
                Nothing -> throwM . ShaderTypeError $ "Error loading uniform " ++ T.unpack n
                Just l  -> return l
    return $ ShaderVar n loc' ty sz

getAttribLocation :: (MonadIO m) => Program -> Text -> m (Maybe AttribLocation)
getAttribLocation p name = do 
    loc <- liftIO . withGLstring name $ glGetAttribLocation (fromProgram p)
    return $ if loc >= 0 then Just . AttribLocation $ fromIntegral loc else Nothing

getUniformLocation :: (MonadIO m) => Program -> Text -> m (Maybe UniformLocation)
getUniformLocation p name = do
    loc <- liftIO . withGLstring name $ glGetUniformLocation (fromProgram p)
    return $ if loc >= 0 then Just . UniformLocation $ fromIntegral loc else Nothing





getActive :: (MonadIO m) => GLsizei -> (Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ()) -> m (Text, GLenum, GLint)
getActive ml f = liftIO . alloca $ 
    \szb -> alloca $
    \tyb -> do
        n <- getGLstring ml (\nlb nb -> f nlb szb tyb nb)
        ty <- peek tyb
        sz <- peek szb
        return $ (n, ty, sz)

getProgramiv :: (MonadIO m, Num a) => Program -> GLenum -> m a
getProgramiv = getGLiv . glGetProgramiv . fromProgram

getShaderiv :: (MonadIO m, Num a) => Shader -> GLenum -> m a
getShaderiv = getGLiv . glGetShaderiv . fromShader

getIntegerv :: (MonadIO m, Num a) => GLenum -> m a
getIntegerv = getGLiv glGetIntegerv

getGLiv :: (MonadIO m, Storable a, Integral a, Num c) => (GLenum -> Ptr a -> IO b) -> GLenum -> m c
getGLiv f enum = liftIO . alloca $ 
    \ptr -> f enum ptr >> liftM fromIntegral (peek ptr)





