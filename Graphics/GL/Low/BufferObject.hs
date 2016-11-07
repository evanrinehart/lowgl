{-# LANGUAGE ScopedTypeVariables #-}

-- | Buffer Objects are objects for holding arbitrary blobs of bytes. This
-- library exposes two types of buffer objects: VBOs and ElementArrays.

module Graphics.GL.Low.BufferObject (

-- * VBO

-- | Vertex Buffer Objects (VBO) contain data for a sequence of vertices. A
-- vertex shader interprets the data for each vertex by mapping the attributes
-- of the vertex (position, normal vector, etc) to input variables using the
-- VAO. VBOs have the data which is used as input to the vertex shader
-- according to the configuration of the VAO.
--
-- Example VBO contents:
--
-- <<vbo.png VBO diagram>>
--
-- The shader will interpret those parts of the VBO as illustrated only after
-- appropriately configuring a VAO. See "Graphics.GL.Low.VAO".

-- * ElementArray

-- | Element arrays are buffer objects that contain a sequence of indices. When
-- using indexed rendering, the bound element array determines the order that
-- the vertices in the VBOs are visited to construct primitives. This allows
-- sharing vertices in cases that many vertices overlap with each other. OpenGL
-- accepts element array objects whose indices are encoded as unsigned bytes,
-- unsigned 16-bit ints, and unsigned 32-bit ints.
--
-- Example ElementArray contents appropriate for rendering triangles and lines
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
  deleteBufferObject,
  bindVBO,
  updateVBO,
  bindElementArray,
  updateElementArray,
  BufferObject,
  UsageHint(..)
) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Data.Word

import Graphics.GL

import Graphics.GL.Low.Types
import Graphics.GL.Low.Classes


-- | Usage hint for allocation of buffer object storage.
data UsageHint = StaticDraw  -- ^ Data will seldomly change.
               | DynamicDraw -- ^ Data will change.
               | StreamDraw  -- ^ Data will change very often.
                 deriving Show

instance ToGL UsageHint where
  toGL StaticDraw  = GL_STATIC_DRAW
  toGL DynamicDraw = GL_DYNAMIC_DRAW
  toGL StreamDraw  = GL_STREAM_DRAW

-- | Use this to create VBOs or element arrays from raw data.
newBufferObject :: Storable a => Vector a -> UsageHint -> IO BufferObject
newBufferObject = newBufferObjectNonClobbering GL_ARRAY_BUFFER

newBufferObjectNonClobbering :: forall a . Storable a => GLenum -> Vector a -> UsageHint -> IO BufferObject
newBufferObjectNonClobbering target src usage = do
  orig <- getArrayBufferBinding
  n <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer target n
  let (fptr, off, len) = V.unsafeToForeignPtr src
  let size = sizeOf (undefined :: a)
  withForeignPtr fptr $ \ptr -> glBufferData
    target
    (fromIntegral (len * size))
    (castPtr (ptr `plusPtr` off))
    (toGL usage)
  glBindBuffer target (fromIntegral orig)
  return (BufferObject n)

getArrayBufferBinding :: IO GLint
getArrayBufferBinding = alloca $ \ptr -> do
  glGetIntegerv GL_ARRAY_BUFFER_BINDING ptr
  peek ptr

-- | Modify the data in the currently bound VBO starting from the specified
-- index in bytes.
updateVBO :: Storable a => Vector a -> Int -> IO ()
updateVBO = updateBufferObject GL_ARRAY_BUFFER

-- | Modify contents in the currently bound element array starting at the
-- specified index in bytes.
updateElementArray :: Storable a => Vector a -> Int -> IO ()
updateElementArray = updateBufferObject GL_ELEMENT_ARRAY_BUFFER

updateBufferObject :: forall a . Storable a => GLenum -> Vector a -> Int -> IO ()
updateBufferObject target src offset = do
  let (fptr, off, len) = V.unsafeToForeignPtr src
  let size = sizeOf (undefined :: a)
  withForeignPtr fptr $ \ptr -> glBufferSubData
    target
    (fromIntegral offset)
    (fromIntegral (len * size))
    (castPtr (ptr `plusPtr` off))

-- | Delete a buffer object.
deleteBufferObject :: BufferObject -> IO ()
deleteBufferObject bo = withArray [fromBufferObject bo] (\ptr -> glDeleteBuffers 1 ptr)

-- | Bind a VBO to the array buffer binding target. The buffer object bound
-- there will be replaced, if any.
bindVBO :: BufferObject -> IO ()
bindVBO = glBindBuffer GL_ARRAY_BUFFER . fromBufferObject

-- | Assign an element array to the element array binding target. It will
-- replace the buffer object already bound there, if any. The binding will
-- be remembered in the currently bound VAO.
bindElementArray :: BufferObject -> IO ()
bindElementArray = glBindBuffer GL_ELEMENT_ARRAY_BUFFER . fromBufferObject
