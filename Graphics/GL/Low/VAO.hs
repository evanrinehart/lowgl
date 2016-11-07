
module Graphics.GL.Low.VAO (

-- | Vertex Array Objects (VAO) are at the core of controlling OpenGL. Each VAO
-- has mutable state which associates vertex shader input variables with three
-- things:
--
-- - The VBO to read from.
-- - Where in the VBO to read from.
-- - The interpretation of the bytes found there (32-bit float, 16-bit int, etc).
--
-- You set these VAO parameters with a sequence of commands:
--
-- - Bind a VAO
-- - Bind a VBO
-- - Use a Program
-- - Call 'Graphics.GL.Low.VertexAttrib.setVertexLayout'
--
-- After a VAO is configured against a Program, either one can be swapped in or
-- out freely. They will still work when both are swapped back in together. An
-- "in-use" shader program will use whatever VAO is bound for getting its
-- vertex inputs. It is up to the programmer to ensure that the VAO has been
-- configured with the right variable positions for the current shader.
--
-- After being bound, VAOs will also remember the last element array that is
-- bound to the element array buffer binding target. Later on, when a VAO is
-- re-bound, the element array will be restored automatically.
--
-- __Gotcha:__ VAOs do /not/ remember what is bound to the array buffer binding
-- target. So VBOs will not be restored when a VAO is bound.
--
-- A VAO must be created and bound with `bindVAO` before you can see any
-- graphics.
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

import Graphics.GL

import Graphics.GL.Low.Types
import Graphics.GL.Low.Classes

-- | Create a new VAO. The only thing you can do with a VAO is bind it to
-- the vertex array binding target (bindVAO) or delete it.
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
