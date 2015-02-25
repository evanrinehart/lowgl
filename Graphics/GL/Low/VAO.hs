
module Graphics.GL.Low.VAO (

-- | Vertex Array Objects (VAO). Despite having almost no operations of its
-- own, the VAO mechanism is one of the most complex pieces of OpenGL. A VAO
-- has mutable state which associates vertex shader input variables (actually
-- the integer location of the variable) with three things:
--
-- - The VBO to read from.
-- - The position in the vertex array to read from.
-- - The interpretation of the bytes found there (32-bit float, 16-bit int, etc).
--
-- You set these VAO parameters with the following dance:
--
-- - Bind a VAO
-- - Bind a VBO
-- - Use a Program
-- - call 'Graphics.GL.Low.VertexAttrib.setVertexLayout' which will
-- lookup the position of the input variables and issue the appropriate
-- glVertexAttribPointer calls.
--
-- After a VAO is configured against a Program, either can be swapped in or
-- out freely and they will still work when both are swapped in again together.
--
-- An "in-use" program will use whatever VAO is bound for getting its vertex
-- inputs. It is up to the programmer to ensure that the VAO has been
-- configured with the right variable positions for the current program. Two
-- ways to do this are to use a consistent set of variables for all shaders or
-- restrict a set of VAOs to only be allowed with specific Program objects.
--
-- The currently bound VBO is not remembered or restored by binding a VAO, but
-- the currently bound ElementArray is. This detail won't affect you if you
-- always explicitly bind an ElementArray after binding a VAO. Alternatively
-- you can exploit this to bundle particular models and their element arrays
-- as a VAO.
--
-- Diagram of possible VAO contents:
--
-- <<vao.png VAO Diagram>>
--
-- The above VAO would be compatible with the following vertex program:
--
-- @
-- #version 150
-- 
-- in vec2 position;
-- in vec3 color;
--
-- out vec3 Color;
-- 
-- void main()
-- {
--     gl_Position = vec4(position, 0.0, 1.0);
--     Color = color;
-- }
-- @

  newVAO,
  deleteVAO,
  bindVAO,
  VAO
) where

import Foreign.Storable
import Foreign.Marshal
import Control.Monad.IO.Class

import Graphics.GL

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes

-- | Create a new VAO. The only thing you can do with a VAO is bind it to
-- the vertex array binding target.
newVAO :: (MonadIO m) => m VAO
newVAO = liftIO $ do
  n <- alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)
  return (VAO n)

-- | Delete a VAO.
deleteVAO :: (MonadIO m) => VAO -> m ()
deleteVAO (VAO n) = liftIO $ withArray [n] (\ptr -> glDeleteVertexArrays 1 ptr)

-- | Assign the VAO to the vertex array binding target. The VAO already bound
-- will be replaced, if any.
bindVAO :: (MonadIO m) => VAO -> m ()
bindVAO (VAO n) = glBindVertexArray n
