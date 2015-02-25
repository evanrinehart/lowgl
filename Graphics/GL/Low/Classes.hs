module Graphics.GL.Low.Classes where

import Graphics.GL

-- | OpenGL internal image formats.
class InternalFormat a where
  internalFormat :: (Eq b, Num b) => proxy a -> b

-- | The allowed attachment point for images with an internal format.
class InternalFormat a => Attachable a where
  attachPoint :: (Eq b, Num b) => proxy a -> b

-- | Textures are GL objects.
class GLObject a => Texture a where

-- | Framebuffers can be bound to the framebuffer binding target. There is
-- a default framebuffer and the client may create an arbitrary number of
-- new framebuffer objects.
class Framebuffer a where
  framebufferName :: Num b => a -> b

class GLObject a => BufferObject a where

-- | Mappable to GL enums.
class ToGL a where
  toGL :: (Num b, Eq b) => a -> b

instance ToGL Bool where
  toGL True = GL_TRUE
  toGL False = GL_FALSE



-- | All GL objects have some numeric name.
class GLObject a where
  glObjectName :: Num b => a -> b

