{-# LANGUAGE PatternSynonyms #-}
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


-- | Mappable to GL enums.
class ToGL a where
  toGL :: (Num b, Eq b) => a -> b

instance ToGL Bool where
  toGL True = GL_TRUE
  toGL False = GL_FALSE


class FromGL a where
    fromGL :: GLenum -> Maybe a
    
    fromGLWith :: a -> GLenum -> a
    fromGLWith def e = case fromGL e of
        Just x  -> x
        Nothing -> def
    
    fromGL' :: GLenum -> a
    fromGL' e = fromGLWith (error $ "unexpected GLenum value " ++ show e) e

instance FromGL Bool where
    fromGL GL_TRUE  = Just True
    fromGL GL_FALSE = Just False
    fromGL _        = Nothing


-- | All GL objects have some numeric name.
class GLObject a where
  glObjectName :: Num b => a -> b

