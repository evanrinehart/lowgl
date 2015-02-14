module Graphics.GL.Low.Render (
  Culling(..),
  Viewport(..),
  IndexFormat(..),
  drawPoints,
  drawLines,
  drawLineStrip,
  drawLineLoop,
  drawTriangles,
  drawTriangleStrip,
  drawTriangleFan,
  drawIndexedPoints,
  drawIndexedLines,
  drawIndexedLineStrip,
  drawIndexedLineLoop,
  drawIndexedTriangles,
  drawIndexedTriangleStrip,
  drawIndexedTriangleFan,
  enableScissorTest,
  disableScissorTest,
  enableCulling,
  disableCulling,
  setViewport
) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Graphics.GL

import Graphics.GL.Low.Classes

-- | Facet culling modes.
data Culling =
  CullFront |
  CullBack |
  CullFrontAndBack
    deriving Show

instance ToGL Culling where
  toGL CullFront = GL_FRONT
  toGL CullBack = GL_BACK
  toGL CullFrontAndBack = GL_FRONT_AND_BACK

-- | A rectangular section of the window.
data Viewport = Viewport
  { viewportX :: Int
  , viewportY :: Int
  , viewportW :: Int
  , viewportH :: Int }
    deriving (Eq, Show)

-- | How indices are packed in an ElementArray buffer object.
data IndexFormat =
  UByteIndices  | -- ^ Each index is one unsigned byte.
  UShortIndices | -- ^ Each index is a two byte unsigned int.
  UIntIndices     -- ^ Each index is a four byte unsigned int.
    deriving Show

instance ToGL IndexFormat where
  toGL UByteIndices  = GL_UNSIGNED_BYTE
  toGL UShortIndices = GL_UNSIGNED_SHORT
  toGL UIntIndices   = GL_UNSIGNED_INT



drawPoints :: Int -> IO ()
drawPoints = drawArrays GL_POINTS

drawLines :: Int -> IO ()
drawLines = drawArrays GL_LINES

drawLineStrip :: Int -> IO ()
drawLineStrip = drawArrays GL_LINE_STRIP

drawLineLoop :: Int -> IO ()
drawLineLoop = drawArrays GL_LINE_LOOP

drawTriangles :: Int -> IO ()
drawTriangles = drawArrays GL_TRIANGLES

drawTriangleStrip :: Int -> IO ()
drawTriangleStrip = drawArrays GL_TRIANGLE_STRIP

drawTriangleFan :: Int -> IO ()
drawTriangleFan = drawArrays GL_TRIANGLE_FAN

drawArrays :: GLenum -> Int -> IO ()
drawArrays mode n = glDrawArrays mode (fromIntegral n) 0

drawIndexedPoints :: Int -> IndexFormat -> IO ()
drawIndexedPoints = drawIndexed GL_POINTS

drawIndexedLines :: Int -> IndexFormat -> IO ()
drawIndexedLines = drawIndexed GL_LINES

drawIndexedLineStrip :: Int -> IndexFormat -> IO ()
drawIndexedLineStrip = drawIndexed GL_LINE_STRIP

drawIndexedLineLoop :: Int -> IndexFormat -> IO ()
drawIndexedLineLoop = drawIndexed GL_LINE_LOOP

drawIndexedTriangles :: Int -> IndexFormat -> IO ()
drawIndexedTriangles = drawIndexed GL_TRIANGLES

drawIndexedTriangleStrip :: Int -> IndexFormat -> IO ()
drawIndexedTriangleStrip = drawIndexed GL_TRIANGLE_STRIP

drawIndexedTriangleFan :: Int -> IndexFormat -> IO ()
drawIndexedTriangleFan = drawIndexed GL_TRIANGLE_FAN

drawIndexed :: GLenum -> Int -> IndexFormat -> IO ()
drawIndexed mode n fmt = glDrawElements mode (fromIntegral n) (toGL fmt) nullPtr

-- | Enable the scissor test. Graphics outside the scissor box will not be
-- rendered.
enableScissorTest :: Viewport -> IO ()
enableScissorTest (Viewport x y w h) = do
  glScissor (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  glEnable GL_SCISSOR_TEST

-- | Disable the scissor test.
disableScissorTest :: IO ()
disableScissorTest = glDisable GL_SCISSOR_TEST


-- | Enable facet culling. The argument specifies whether front faces, back
-- faces, or both will be omitted from rendering. If both front and back
-- faces are culled you can still render points and lines.
enableCulling :: Culling -> IO ()
enableCulling c = do
  case c of
    CullFront -> glCullFace GL_FRONT
    CullBack -> glCullFace GL_BACK
    CullFrontAndBack -> glCullFace GL_FRONT_AND_BACK
  glEnable GL_CULL_FACE

-- | Disable facet culling. Front and back faces will now be rendered.
disableCulling :: IO ()
disableCulling = glDisable GL_CULL_FACE

-- | Set the viewport. The default viewport simply covers the entire window.
setViewport :: Viewport -> IO ()
setViewport (Viewport x y w h) =
  glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
