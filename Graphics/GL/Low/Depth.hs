module Graphics.GL.Low.Depth where

import Graphics.GL

-- | Enable the depth test. Attempting to render pixels with a depth value
-- greater than the depth buffer at those pixels will have no effect. Otherwise
-- the depth in the buffer will get updated to the new pixel's depth.
enableDepthTest :: IO ()
enableDepthTest = glEnable GL_DEPTH_TEST

-- | Disable the depth test and depth buffer updates.
disableDepthTest :: IO ()
disableDepthTest = glDisable GL_DEPTH_TEST

-- | Clear the depth buffer with the maximum depth value.
clearDepthBuffer :: IO ()
clearDepthBuffer = glClear GL_DEPTH_BUFFER_BIT
