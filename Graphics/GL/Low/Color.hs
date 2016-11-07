module Graphics.GL.Low.Color where

import Graphics.GL

-- | Allow rendering commands to modify the color buffer of the current
-- framebuffer.
enableColorWriting :: IO ()
enableColorWriting = glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE

-- | Disable rendering to color buffer.
disableColorWriting :: IO ()
disableColorWriting = glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE

-- | Clear the color buffer of the current framebuffer with the specified
-- color. Has no effect if writing to the color buffer is disabled.
clearColorBuffer :: Real a => (a,a,a) -> IO ()
clearColorBuffer (r, g, b) = do
  glClearColor (realToFrac r) (realToFrac g) (realToFrac b) 1.0
  glClear GL_COLOR_BUFFER_BIT
