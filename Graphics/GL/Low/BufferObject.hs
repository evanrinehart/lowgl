{-# LANGUAGE ScopedTypeVariables #-}

-- | Buffer Objects are objects for holding arbitrary blobs of bytes. This
-- library exposes two types of buffer objects: VBOs and ElementArrays.

module Graphics.GL.Low.BufferObject (

-- * VBO

-- | Vertex Buffer Objects (VBO) contain data for a sequence of vertices. A
-- vertex shader interprets the data for each vertex by mapping the attributes
-- of the vertex (position, normal vector, etc) to input variables using the
-- VAO. VBOs have the data which is used as input to the vertex shader
-- according to the configuration of the VAO/.
--
-- Example VBO contents:
--
-- <<vbo.png VBO diagram>>
--
-- The shader will interpret those parts of the VBO as illustrated only after
-- appropiately configuring a VAO. See "Graphics.GL.Low.VAO".

-- * ElementArray

-- | Element arrays are buffer objects that contain a sequence of indices. When
-- using indexed rendering, the bound element array determines the order that
-- the vertices in the VBOs are visited to construct primitives. This allows
-- sharing vertices in cases that many vertices overlap with each other. OpenGL
-- accepts element array objects whose indices are encoded as unsigned bytes,
-- unsigned 16-bit ints, and unsigned 32-bit ints.
--
-- Example ElementArray contents appropriate for render triangles and lines
-- respectively:
--
-- <<element_array.png Element array diagram>>
--
-- See 'Graphics.GL.Low.Render.drawIndexedTriangles' and friends to see which
-- primitives are possible to construct with element arrays. It is not
-- necessary to use element arrays to render. The non-indexed versions of
-- the primitive render commands will simply traverse the vertices in order
-- specified in the VBOs.

-- * Documentation

  newBufferObject,
  bindVBO,
  loadVBO,
  updateVBO,
  bindElementArray,
  loadElementArray,
  updateElementArray,
  deleteBufferObject,
  VBO,
  ElementArray,
  BufferObject(..),
  UsageHint(..)
) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Data.Word
import Control.Monad.IO.Class
import Control.Monad (liftM)

import Graphics.GL

import Graphics.GL.Low.Internal.BufferObject
import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes

-- | Create a new buffer object.
newBufferObject :: MonadIO m => m BufferObject
newBufferObject = liftM BufferObject gen where
  gen = liftIO $ alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)

-- | Delete a VBO or ElementArray.
deleteBufferObject :: MonadIO m => BufferObject -> m ()
deleteBufferObject bo = liftIO $
  withArray [fromBufferObject bo] (\ptr -> glDeleteBuffers 1 ptr)

-- | Bind a VBO to the array buffer binding target. The buffer object bound
-- there will be replaced, if any.
bindVBO :: (MonadIO m) => VBO -> m ()
bindVBO (BufferObject n) = glBindBuffer GL_ARRAY_BUFFER n

-- | Allocate storage for and load a blob of data into the currently bound VBO.
loadVBO :: (MonadIO m, Storable a) => Vector a -> UsageHint -> m ()
loadVBO = loadBufferObject GL_ARRAY_BUFFER

-- | Modify the data in the currently bound VBO starting from the specified
-- index in bytes.
updateVBO :: (MonadIO m, Storable a) => Vector a -> Int -> m ()
updateVBO = updateBufferObject GL_ARRAY_BUFFER

-- | Assign an ElementArray to the element array binding target. It will
-- replace the ElementArray already bound there, if any. Note that the state
-- of the element array binding target is a function of the current VAO.
bindElementArray :: (MonadIO m) => ElementArray -> m ()
bindElementArray (BufferObject n) = glBindBuffer GL_ELEMENT_ARRAY_BUFFER n

-- | Allocate storage for and load a blob of data into the currently bound
-- ElementArray.
loadElementArray :: (MonadIO m, Storable a) => Vector a -> UsageHint -> m ()
loadElementArray = loadBufferObject GL_ELEMENT_ARRAY_BUFFER

-- | Modify contents in the currently bound ElementArray starting at the
-- specified index in bytes.
updateElementArray :: (MonadIO m, Storable a) => Vector a -> Int -> m ()
updateElementArray = updateBufferObject GL_ELEMENT_ARRAY_BUFFER

