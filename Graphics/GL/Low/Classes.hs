module Graphics.GL.Low.Classes where

import Graphics.GL

-- | Mappable to GL enums.
class ToGL a where
  toGL :: (Num b, Eq b) => a -> b

instance ToGL Bool where
  toGL True  = GL_TRUE
  toGL False = GL_FALSE
