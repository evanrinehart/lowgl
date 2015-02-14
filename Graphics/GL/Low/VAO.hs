module Graphics.GL.Low.VAO 
(
  VAO,
  newVAO,
  deleteVAO,
  bindVAO
) where

import Foreign.Storable
import Foreign.Marshal

import Graphics.GL

import Graphics.GL.Low.Classes

-- | A VAO stores vertex attribute layouts and the VBO source of vertices
-- for those attributes. It also stores the state of the element array binding
-- target. The vertex array binding target admits one VAO at a time.
newtype VAO = VAO GLuint deriving Show

instance GLObject VAO where
  glObjectName (VAO n) = fromIntegral n

-- | Create a new VAO. The only thing you can do with a VAO is bind it to
-- the vertex array binding target.
newVAO :: IO VAO
newVAO = do
  n <- alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)
  return (VAO n)

-- | Delete a VAO.
deleteVAO :: VAO -> IO ()
deleteVAO (VAO n) = withArray [n] (\ptr -> glDeleteVertexArrays 1 ptr)

-- | Assign the VAO to the vertex array binding target. The VAO already bound
-- will be replaced, if any.
bindVAO :: VAO -> IO ()
bindVAO (VAO n) = glBindVertexArray n
