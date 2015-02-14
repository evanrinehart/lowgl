{-# LANGUAGE RankNTypes #-}
module Graphics.GL.Low.Framebuffer (
  FBO,
  RBO,
  DefaultFramebuffer,
  bindFramebuffer,
  newFBO,
  deleteFBO,
  attachTex2D,
  attachCubeMap,
  attachRBO,
  newRBO,
  deleteRBO
) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Data.Default
import Graphics.GL

import Graphics.GL.Low.Classes
import Graphics.GL.Low.Common
import Graphics.GL.Low.Cube
import Graphics.GL.Low.Texture


-- | A framebuffer object is an alternative rendering destination. Once an FBO
-- is bound to framebuffer binding target, it is possible to attach images
-- (textures or RBOs) for color, depth, or stencil rendering.
newtype FBO = FBO GLuint deriving Show

-- | An RBO is a kind of image object used for rendering. The only thing
-- you can do with an RBO is attach it to an FBO.
data RBO a = RBO { unRBO :: GLuint } deriving Show

-- | The default framebuffer. Bind this to render to the screen as usual.
-- Use the Default instance method 'def' to construct it.
data DefaultFramebuffer = DefaultFramebuffer deriving Show

instance Default DefaultFramebuffer where
  def = DefaultFramebuffer

instance Framebuffer DefaultFramebuffer where
  framebufferName _ = 0

instance Framebuffer FBO where
  framebufferName = glObjectName

instance GLObject FBO where
  glObjectName (FBO n) = fromIntegral n

instance GLObject (RBO a) where
  glObjectName (RBO n) = fromIntegral n

-- | Binds an FBO or the default framebuffer to the framebuffer binding target.
-- Replaces the framebuffer already bound there.
bindFramebuffer :: Framebuffer a => a -> IO ()
bindFramebuffer x = glBindFramebuffer GL_FRAMEBUFFER (framebufferName x)

-- | Create a new framebuffer object. Before the framebuffer can be used for
-- rendering it must have a color image attachment.
newFBO :: IO FBO
newFBO = do
  n <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
  return (FBO n)

-- | Delete an FBO.
deleteFBO :: FBO -> IO ()
deleteFBO (FBO n) = withArray [n] (\ptr -> glDeleteFramebuffers 1 ptr)

-- | Attach a 2D texture to the FBO currently bound to the
-- framebuffer binding target.
attachTex2D :: Attachable a => Tex2D a -> IO ()
attachTex2D tex =
  glFramebufferTexture2D
    GL_FRAMEBUFFER
    (attachPoint tex)
    GL_TEXTURE_2D
    (glObjectName tex)
    0

-- | Attach one of the sides of a cubemap texture to the FBO currently bound
-- to the framebuffer binding target.
attachCubeMap :: Attachable a => CubeMap a -> Side -> IO ()
attachCubeMap cm side =
  glFramebufferTexture2D
    GL_FRAMEBUFFER
    (attachPoint cm)
    (side cubeSideCodes)
    (glObjectName cm)
    0

-- | Attach an RBO to the FBO currently bound to the framebuffer binding
-- target.
attachRBO :: Attachable a => RBO a -> IO ()
attachRBO rbo = glFramebufferRenderbuffer
  GL_FRAMEBUFFER (attachPoint rbo) GL_RENDERBUFFER (unRBO rbo)

-- | Create a new renderbuffer with the specified dimensions.
newRBO :: InternalFormat a => Int -> Int -> IO (RBO a)
newRBO w h = do
  n <- alloca (\ptr -> glGenRenderbuffers 1 ptr >> peek ptr)
  rbo <- return (RBO n)
  glBindRenderbuffer GL_RENDERBUFFER n
  glRenderbufferStorage
    GL_RENDERBUFFER
    (internalFormat rbo)
    (fromIntegral w)
    (fromIntegral h)
  return rbo

-- | Delete an RBO.
deleteRBO :: RBO a -> IO ()
deleteRBO (RBO n) = withArray [n] (\ptr -> glDeleteRenderbuffers 1 ptr)
