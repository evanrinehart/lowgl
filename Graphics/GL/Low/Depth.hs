module Graphics.GL.Low.Depth where

import Control.Monad.IO.Class
import Graphics.GL

-- | Enable the depth test. Attempting to render pixels with a depth value
-- greater than the depth buffer at those pixels will have no effect. Otherwise
-- the depth in the buffer will get updated to the new pixel's depth.
enableDepthTest :: (MonadIO m) => m ()
enableDepthTest = glEnable GL_DEPTH_TEST

-- | Disable the depth test and depth buffer updates.
disableDepthTest :: (MonadIO m) => m ()
disableDepthTest = glDisable GL_DEPTH_TEST

-- | Clear the depth buffer with the maximum depth value.
clearDepthBuffer :: (MonadIO m) => m ()
clearDepthBuffer = glClear GL_DEPTH_BUFFER_BIT
