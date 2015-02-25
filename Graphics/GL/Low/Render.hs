module Graphics.GL.Low.Render (

  -- * Primitives
  --
  -- | Render various kinds of primitives to the current framebuffer using
  -- the current shader program. The integer argument is the number of
  -- vertices to read from the VBOs via the current VAO.
  --
  drawPoints,
  drawLines,
  drawLineStrip,
  drawLineLoop,
  drawTriangles,
  drawTriangleStrip,
  drawTriangleFan,

  -- * Primitives (by index)
  --
  -- | Render various kinds of primitives by traversing the vertices in the
  -- order specified in the current ElementArray. The format argument indicates
  -- the size of each index in the ElementArray.
  --
  drawIndexedPoints,
  drawIndexedLines,
  drawIndexedLineStrip,
  drawIndexedLineLoop,
  drawIndexedTriangles,
  drawIndexedTriangleStrip,
  drawIndexedTriangleFan,

  -- * Scissor Test
  enableScissorTest,
  disableScissorTest,

  -- * Facet Culling
  enableCulling,
  disableCulling,

  -- * Viewport
  setViewport,

  Culling(..),
  Viewport(..),
  IndexFormat(..)
) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.IO.Class

import Graphics.GL

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes

-- | A rectangular section of the window.
data Viewport = Viewport
  { viewportX :: Int
  , viewportY :: Int
  , viewportW :: Int
  , viewportH :: Int }
    deriving (Eq, Show)


drawPoints :: (MonadIO m) => Int -> m ()
drawPoints = drawArrays GL_POINTS

drawLines :: (MonadIO m) => Int -> m ()
drawLines = drawArrays GL_LINES

drawLineStrip :: (MonadIO m) => Int -> m ()
drawLineStrip = drawArrays GL_LINE_STRIP

drawLineLoop :: (MonadIO m) => Int -> m ()
drawLineLoop = drawArrays GL_LINE_LOOP

drawTriangles :: (MonadIO m) => Int -> m ()
drawTriangles = drawArrays GL_TRIANGLES

drawTriangleStrip :: (MonadIO m) => Int -> m ()
drawTriangleStrip = drawArrays GL_TRIANGLE_STRIP

drawTriangleFan :: (MonadIO m) => Int -> m ()
drawTriangleFan = drawArrays GL_TRIANGLE_FAN

drawArrays :: (MonadIO m) => GLenum -> Int -> m ()
drawArrays mode n = glDrawArrays mode 0 (fromIntegral n)

drawIndexedPoints :: (MonadIO m) => Int -> IndexFormat -> m ()
drawIndexedPoints = drawIndexed GL_POINTS

drawIndexedLines :: (MonadIO m) => Int -> IndexFormat -> m ()
drawIndexedLines = drawIndexed GL_LINES

drawIndexedLineStrip :: (MonadIO m) => Int -> IndexFormat -> m ()
drawIndexedLineStrip = drawIndexed GL_LINE_STRIP

drawIndexedLineLoop :: (MonadIO m) => Int -> IndexFormat -> m ()
drawIndexedLineLoop = drawIndexed GL_LINE_LOOP

drawIndexedTriangles :: (MonadIO m) => Int -> IndexFormat -> m ()
drawIndexedTriangles = drawIndexed GL_TRIANGLES

drawIndexedTriangleStrip :: (MonadIO m) => Int -> IndexFormat -> m ()
drawIndexedTriangleStrip = drawIndexed GL_TRIANGLE_STRIP

drawIndexedTriangleFan :: (MonadIO m) => Int -> IndexFormat -> m ()
drawIndexedTriangleFan = drawIndexed GL_TRIANGLE_FAN

drawIndexed :: (MonadIO m) => GLenum -> Int -> IndexFormat -> m ()
drawIndexed mode n fmt = glDrawElements mode (fromIntegral n) (toGL fmt) nullPtr

-- | Enable the scissor test. Graphics outside the scissor box will not be
-- rendered.
enableScissorTest :: (MonadIO m) => Viewport -> m ()
enableScissorTest (Viewport x y w h) = do
  glScissor (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  glEnable GL_SCISSOR_TEST

-- | Disable the scissor test.
disableScissorTest :: (MonadIO m) => m ()
disableScissorTest = glDisable GL_SCISSOR_TEST


-- | Enable facet culling. The argument specifies whether front faces, back
-- faces, or both will be omitted from rendering. If both front and back
-- faces are culled you can still render points and lines.
enableCulling :: (MonadIO m) => Culling -> m ()
enableCulling c = do
  case c of
    CullFront -> glCullFace GL_FRONT
    CullBack -> glCullFace GL_BACK
    CullFrontAndBack -> glCullFace GL_FRONT_AND_BACK
  glEnable GL_CULL_FACE

-- | Disable facet culling. Front and back faces will now be rendered.
disableCulling :: (MonadIO m) => m ()
disableCulling = glDisable GL_CULL_FACE

-- | Set the viewport. The default viewport simply covers the entire window.
setViewport :: (MonadIO m) => Viewport -> m ()
setViewport (Viewport x y w h) =
  glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
