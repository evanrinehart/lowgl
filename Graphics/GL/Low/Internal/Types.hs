{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GL.Low.Internal.Types where

import Data.Data (Data, Typeable)
import Foreign.Storable (Storable)

import Graphics.GL (GLuint)

import Graphics.GL.Low.Classes


newtype TextureUnit = TextureUnit { fromTextureUnit :: GLuint } 
    deriving (Eq, Ord, Read, Show, Num, Integral, Real, Enum, Storable)

newtype AttribLocation = AttribLocation { fromAttribLocation :: GLuint } 
    deriving (Eq, Ord, Read, Show, Num, Integral, Real, Enum, Storable)

newtype UniformLocation = UniformLocation { fromUniformLocation :: GLuint } 
    deriving (Eq, Ord, Read, Show, Num, Integral, Real, Enum, Storable)


-- | Handle to a shader program.
newtype Program = Program { fromProgram :: GLuint } 
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)

-- | Handle to a shader object.
newtype Shader = Shader { fromShader :: GLuint } 
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)


-- | Handle to a buffer object.
newtype BufferObject = BufferObject { fromBufferObject :: GLuint } deriving Show
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)

instance GLObject BufferObject where
  glObjectName (BufferObject n) = fromIntegral n

-- | Handle to a VBO.
type VBO = BufferObject

-- | Handle to an element array buffer object.
type ElementArray = BufferObject




-- | A framebuffer object is an alternative rendering destination. Once an FBO
-- is bound to framebuffer binding target, it is possible to attach images
-- (textures or RBOs) for color, depth, or stencil rendering.
newtype FBO = FBO { fromFBO :: GLuint }
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)

instance Framebuffer FBO where
  framebufferName = glObjectName

instance GLObject FBO where
  glObjectName (FBO n) = fromIntegral n


-- | An RBO is a kind of image object used for rendering. The only thing
-- you can do with an RBO is attach it to an FBO.
newtype RBO a = RBO { unRBO :: GLuint } 
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)

instance GLObject (RBO a) where
  glObjectName (RBO n) = fromIntegral n


-- | A 2D texture. A program can sample a texture if it has been bound to
-- the appropriate texture unit.
newtype Tex2D a = Tex2D { fromTex2D :: GLuint }
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)

instance Texture (Tex2D a) where

instance GLObject (Tex2D a) where
  glObjectName (Tex2D n) = fromIntegral n


-- | A cubemap texture is just six 2D textures. A program can sample a cubemap
-- texture if it has been bound to the appropriate texture unit.
newtype CubeMap a = CubeMap { fromCubeMap :: GLuint }
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)

instance Texture (CubeMap a) where

instance GLObject (CubeMap a) where
  glObjectName (CubeMap n) = fromIntegral n



-- | Handle to a VAO.
newtype VAO = VAO { fromVAO :: GLuint }
    deriving (Eq, Ord, Read, Show, Storable, Data, Typeable)

instance GLObject VAO where
  glObjectName (VAO n) = fromIntegral n


