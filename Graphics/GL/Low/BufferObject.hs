{-# LANGUAGE ScopedTypeVariables #-}

-- | Buffer Objects are objects for holding arbitrary blobs of bytes. This
-- library exposes two types of buffer objects: VBOs and ElementArrays.
--
-- = VBO
--
-- Vertex Buffer Objects (VBO) contain data for a sequence of vertices. A
-- vertex shader interprets the data for each vertex by mapping the attributes
-- of the vertex (position, normal vector, etc) to input variables using the
-- VAO. /VBOs have the data which is used as input to the vertex shader according to the configuration of the VAO/.
--
-- Example VBO contents:
--
-- <<vbo.png VBO diagram>>
--
-- The shader will interpret those parts of the VBO as illustrated only after
-- appropiately configuring a VAO. See "Graphics.GL.Low.VAO".
--
-- = ElementArray
--
-- Element arrays are buffer objects that contain a sequence of indices. When
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

module Graphics.GL.Low.BufferObject (
  VBO,
  ElementArray,
  UsageHint(..),
  newVBO,
  updateVBO,
  bindVBO,
  newElementArray,
  updateElementArray,
  bindElementArray,
  deleteBufferObject
) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Data.Word

import Graphics.GL

import Graphics.GL.Low.Classes

-- | Handle to a VBO.
data VBO = VBO GLuint deriving Show

-- | Handle to an element array buffer object.
data ElementArray = ElementArray GLuint deriving Show

-- | Usage hint for allocation of buffer object storage.
data UsageHint = StaticDraw  -- ^ Data will seldomly change.
               | DynamicDraw -- ^ Data will change.
               | StreamDraw  -- ^ Data will change very often.
                 deriving Show

instance ToGL UsageHint where
  toGL StaticDraw  = GL_STATIC_DRAW
  toGL DynamicDraw = GL_DYNAMIC_DRAW
  toGL StreamDraw  = GL_STREAM_DRAW

instance GLObject VBO where
  glObjectName (VBO n) = fromIntegral n

instance GLObject ElementArray where
  glObjectName (ElementArray n) = fromIntegral n

instance BufferObject VBO

instance BufferObject ElementArray


-- | Create a buffer object from a blob of bytes. The usage argument hints
-- at how often you will modify the data.
newVBO :: Storable a => Vector a -> UsageHint -> IO VBO
newVBO = newBufferObject VBO GL_ARRAY_BUFFER

-- | Delete a VBO or ElementArray.
deleteBufferObject :: BufferObject a => a -> IO ()
deleteBufferObject bo = withArray [glObjectName bo] (\ptr -> glDeleteBuffers 1 ptr)

-- | Modify the data in the currently bound VBO starting from the specified
-- index in bytes.
updateVBO :: Storable a => Vector a -> Int -> IO ()
updateVBO = updateBufferObject GL_ARRAY_BUFFER

-- | Bind a VBO to the array buffer binding target. The buffer object bound
-- there will be replaced, if any.
bindVBO :: VBO -> IO ()
bindVBO (VBO n) = glBindBuffer GL_ARRAY_BUFFER n


-- | Create a new ElementArray buffer object from the blob of packed indices.
-- The usage argument hints at how often you plan to modify the data.
newElementArray :: Storable a => Vector a -> UsageHint -> IO ElementArray
newElementArray = newBufferObject ElementArray GL_ELEMENT_ARRAY_BUFFER

-- | Modify contents in the currently bound ElementArray starting at the
-- specified index in bytes.
updateElementArray :: Storable a => Vector a -> Int -> IO ()
updateElementArray = updateBufferObject GL_ELEMENT_ARRAY_BUFFER

-- | Assign an ElementArray to the element array binding target. It will
-- replace the ElementArray already bound there, if any. Note that the state
-- of the element array binding target is a function of the current VAO.
bindElementArray :: ElementArray -> IO ()
bindElementArray (ElementArray n) = glBindBuffer GL_ELEMENT_ARRAY_BUFFER n


newBufferObject :: forall a b . Storable a => (GLuint -> b) -> GLenum -> Vector a -> UsageHint -> IO b
newBufferObject ctor target src usage = do
  n <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer target n
  let (fptr, off, len) = V.unsafeToForeignPtr src
  let size = sizeOf (undefined :: a)
  withForeignPtr fptr $ \ptr -> glBufferData
    target
    (fromIntegral (len * size))
    (castPtr (ptr `plusPtr` off))
    (toGL usage)
  return (ctor n)

updateBufferObject :: forall a . Storable a => GLenum -> Vector a -> Int -> IO ()
updateBufferObject target bytes offset = do
  let (fptr, off, len) = V.unsafeToForeignPtr bytes
  let size = sizeOf (undefined :: a)
  withForeignPtr fptr $ \ptr -> glBufferSubData
    target
    (fromIntegral offset)
    (fromIntegral (len * size))
    (castPtr (ptr `plusPtr` off))


