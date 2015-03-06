{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GL.Low.Internal.Texture where

import Prelude hiding (sequence)
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Vector.Storable hiding (mapM_)
import Data.Word
import Control.Applicative
import Data.Traversable (sequence)
import Control.Monad.IO.Class

import Graphics.GL
import Linear

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Internal.Common
import Graphics.GL.Low.Classes


genTexture :: (MonadIO m, Texture a) => m a
genTexture = do
    n <- liftIO . alloca $ \ptr -> glGenTextures 1 ptr >> peek ptr
    return $ glObject n

texImage2D target lv format sz iformat ty bytes = liftIO $ case bytes of
    Nothing -> glTexImage2D target lv format w h 0 iformat ty nullPtr
    Just bs -> unsafeWith bs $  \ptr -> glTexImage2D target lv format w h 0 iformat ty (castPtr ptr)
  where (V2 w h) = fromIntegral <$> sz



-- | Delete a texture.
deleteTexture :: (MonadIO m, Texture a) => a -> m ()
deleteTexture x = liftIO $ withArray [glObjectName x] (\ptr -> glDeleteTextures 1 ptr)

-- | Bind a 2D texture to the 2D texture binding target and the currently
-- active texture unit.
bindTexture2D :: (MonadIO m) => Tex2D a -> m ()
bindTexture2D (Tex2D n) = glBindTexture GL_TEXTURE_2D n

-- | Bind a cubemap texture to the cubemap texture binding target and
-- the currently active texture unit.
bindTextureCubeMap :: (MonadIO m) => CubeMap a -> m ()
bindTextureCubeMap (CubeMap n) = glBindTexture GL_TEXTURE_CUBE_MAP n

-- | Set the active texture unit. The default is zero.
setActiveTextureUnit :: (MonadIO m, Enum a) => a -> m ()
setActiveTextureUnit n =
  (glActiveTexture . fromIntegral) (GL_TEXTURE0 + fromEnum n)

-- | Set the filtering for the 2D texture currently bound to the 2D texture
-- binding target.
setTex2DFiltering :: (MonadIO m) => Filtering -> m ()
setTex2DFiltering filt = do
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (toGL filt)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (toGL filt)

-- | Set the filtering for the cubemap texture currently bound to the cubemap
-- texture binding target.
setCubeMapFiltering :: (MonadIO m) => Filtering -> m ()
setCubeMapFiltering filt = do
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MIN_FILTER (toGL filt)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MAG_FILTER (toGL filt)

-- | Set the wrapping mode for the 2D texture currently bound to the 2D
-- texture binding target.
setTex2DWrapping :: (MonadIO m) => Wrapping -> m ()
setTex2DWrapping wrap = do
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (toGL wrap)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (toGL wrap)

-- | Set the wrapping mode for the cubemap texture currently bound to the
-- cubemap texture binding target. Because no blending occurs between cube
-- faces you probably want ClampToEdge.
setCubeMapWrapping :: (MonadIO m) => Wrapping -> m ()
setCubeMapWrapping wrap = do
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_S (toGL wrap)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_T (toGL wrap)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_R (toGL wrap)

