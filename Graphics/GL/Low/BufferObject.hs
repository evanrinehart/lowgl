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
newVBO :: Vector Word8 -> UsageHint -> IO VBO
newVBO src usage = do
  n <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  let len = V.length src
  glBindBuffer GL_ARRAY_BUFFER n
  V.unsafeWith src $ \ptr -> glBufferData
    GL_ARRAY_BUFFER
    (fromIntegral len)
    (castPtr ptr)
    (toGL usage)
  return (VBO n)

-- | Delete a VBO or ElementArray.
deleteBufferObject :: BufferObject a => a -> IO ()
deleteBufferObject bo = withArray [glObjectName bo] (\ptr -> glDeleteBuffers 1 ptr)

-- | Modify the data in the currently bound VBO starting from the specified
-- index in bytes.
updateVBO :: Vector Word8 -> Int -> IO ()
updateVBO src offset = do
  let len = V.length src
  V.unsafeWith src $ \ptr -> glBufferSubData
    GL_ARRAY_BUFFER 
    (fromIntegral offset)
    (fromIntegral len)
    (castPtr ptr)

-- | Bind a VBO to the array buffer binding target. The buffer object bound
-- there will be replaced, if any.
bindVBO :: VBO -> IO ()
bindVBO (VBO n) = glBindBuffer GL_ARRAY_BUFFER n


-- | Create a new ElementArray buffer object from the blob of packed indices.
-- The usage argument hints at how often you plan to modify the data.
newElementArray :: Vector Word8 -> UsageHint -> IO ElementArray
newElementArray bytes usage = do
  n <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER n
  let len = V.length bytes
  V.unsafeWith bytes $ \ptr -> do
    glBufferData
      GL_ELEMENT_ARRAY_BUFFER
      (fromIntegral len)
      (castPtr ptr)
      (toGL usage)
  return (ElementArray n)

-- | Modify contents in the currently bound ElementArray starting at the
-- specified index in bytes.
updateElementArray :: Vector Word8 -> Int -> IO ()
updateElementArray bytes offset = V.unsafeWith bytes $ \ptr -> do
  glBufferSubData
    GL_ELEMENT_ARRAY_BUFFER
    (fromIntegral offset)
    (fromIntegral (V.length bytes))
    (castPtr ptr)


-- | Assign an ElementArray to the element array binding target. It will
-- replace the ElementArray already bound there, if any. Note that the state
-- of the element array binding target is a function of the current VAO.
bindElementArray :: ElementArray -> IO ()
bindElementArray (ElementArray n) = glBindBuffer GL_ELEMENT_ARRAY_BUFFER n
