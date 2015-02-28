{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GL.Low.Internal.BufferObject where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Control.Monad.IO.Class

import Graphics.GL

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes

loadBufferObject :: forall m a . (MonadIO m, Storable a)
                 => GLenum
                 -> Vector a
                 -> UsageHint
                 -> m ()
loadBufferObject target src usage = do
  let (fptr, off, len) = V.unsafeToForeignPtr src
  let size = sizeOf (undefined :: a)
  liftIO . withForeignPtr fptr $ \ptr -> glBufferData
    target
    (fromIntegral (len * size))
    (castPtr (ptr `plusPtr` off))
    (toGL usage)

updateBufferObject :: forall m a. (MonadIO m, Storable a) => GLenum -> Vector a -> Int -> m ()
updateBufferObject target bytes offset = do
  let (fptr, off, len) = V.unsafeToForeignPtr bytes
  let size = sizeOf (undefined :: a)
  liftIO . withForeignPtr fptr $ \ptr -> glBufferSubData
    target
    (fromIntegral offset)
    (fromIntegral (len * size))
    (castPtr (ptr `plusPtr` off))
