{-# LANGUAGE ScopedTypeVariables #-}
-- | VBO and ElementArrays. Both are buffer objects but are used for two
-- different things.
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

-- | A VBO is a buffer object which has vertex data. Shader programs use VBOs
-- as input to their vertex attributes according to the configuration of the
-- bound VAO.
data VBO = VBO GLuint deriving Show

-- | A buffer object which has a packed sequence of vertex indices. Indexed
-- rendering uses the ElementArray bound to the element array binding target.
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


