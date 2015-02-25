module Graphics.GL.Low.Color where

import Control.Monad.IO.Class
import Graphics.GL

-- | Allow rendering commands to modify the color buffer of the current
-- framebuffer.
enableColorWriting :: (MonadIO m) => m ()
enableColorWriting = glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE

-- | Disable rendering to color buffer.
disableColorWriting :: (MonadIO m) => m ()
disableColorWriting = glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE

-- | Clear the color buffer of the current framebuffer with the specified
-- color. Has no effect if writing to the color buffer is disabled.
clearColorBuffer :: (MonadIO m) => (Float, Float, Float) -> m ()
clearColorBuffer (r, g, b) = do
  glClearColor (realToFrac r) (realToFrac g) (realToFrac b) 1.0
  glClear GL_COLOR_BUFFER_BIT
