module Graphics.GL.Low.Texture (

-- | Textures are objects that contain image data that can be sampled by
-- a shader. An obvious application of this is texture mapping, but there
-- are many other uses for textures. The image data doesn't have to be an
-- image at all, it can represent anything.
--
-- Each sampler uniform in your shader points to a texture unit (zero by
-- default). This texture unit is where it will read texture data from. To
-- assign a texture to a texture unit, use 'setActiveTextureUnit' then bind
-- a texture. This will not only bind it to the relevant texture binding target
-- but also to the active texture unit. You can change which unit a sampler
-- points to by setting it using the 'Graphics.GL.Low.Shader.setUniform1i'
-- command.
--
-- You can avoid dealing with active texture units if theres only one
-- sampler because the default unit is zero.

  newTexture2D,
  newCubeMap,
  newEmptyTexture2D,
  newEmptyCubeMap,
  deleteTexture,
  bindTexture2D,
  bindTextureCubeMap,
  setActiveTextureUnit,
  setTex2DFiltering,
  setCubeMapFiltering,
  setTex2DWrapping,
  setCubeMapWrapping,
  Texture,
  ImageFormat(..),
  Filtering(..),
  Wrapping(..)

  -- * Example
  -- $example

) where

import Prelude hiding (sequence)
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Vector.Storable
import Data.Word
import Control.Applicative
import Data.Traversable (sequence)

import Graphics.GL

import Graphics.GL.Low.Types
import Graphics.GL.Low.Classes
import Graphics.GL.Low.Cube
import Graphics.GL.Low.Common


-- | Create a new 2D texture from raw image data, its dimensions, and the
-- assumed image format. This operation clobbers the tex 2D binding
-- target. The dimensions should be powers of 2.
newTexture2D :: Storable a
             => Vector a
             -> (Int,Int)
             -> ImageFormat
             -> IO Texture
newTexture2D src (w,h) format = do
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_2D n
  unsafeWith src $ \ptr -> glTexImage2D
    GL_TEXTURE_2D
    0
    (toGL format)
    (fromIntegral w)
    (fromIntegral h)
    0
    (toGL format)
    GL_UNSIGNED_BYTE
    (castPtr ptr)
  glGenerateMipmap GL_TEXTURE_2D
  return (Texture n format)

-- | Create a new cubemap texture from six raw data sources. This operation
-- clobbers the cubemap binding target.
newCubeMap :: Storable a
           => Cube (Vector a, (Int,Int))
           -> ImageFormat
           -> IO Texture
newCubeMap images format = do
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_CUBE_MAP n
  sequence_ (liftA2 (loadCubeMapSide format) images cubeSideCodes)
  glGenerateMipmap GL_TEXTURE_CUBE_MAP
  return (Texture n format)
  
loadCubeMapSide :: Storable a => ImageFormat -> (Vector a, (Int,Int)) -> GLenum -> IO ()
loadCubeMapSide format (src, (w,h)) side =
  unsafeWith src $ \ptr -> glTexImage2D
    side
    0
    (toGL format)
    (fromIntegral w)
    (fromIntegral h)
    0
    (toGL format)
    GL_UNSIGNED_BYTE
    (castPtr ptr)

-- | Create an empty texture with the specified dimensions and format.
-- This clobbers the tex 2D binding target.
newEmptyTexture2D :: Int -> Int -> ImageFormat -> IO Texture
newEmptyTexture2D w h format = do
  let w' = fromIntegral w
  let h' = fromIntegral h
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_2D n
  glTexImage2D GL_TEXTURE_2D 0 (toGL format) w' h' 0 (toGL format) GL_UNSIGNED_BYTE nullPtr
  return (Texture n format)

-- | Create a cubemap texture where each of the six sides has the specified
-- dimensions and format. This clobbers the cubemap binding
-- target.
newEmptyCubeMap :: Int -> Int -> ImageFormat -> IO Texture
newEmptyCubeMap w h format = do
  let w' = fromIntegral w
  let h' = fromIntegral h
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  let fmt  = toGL format
  let fmt' = toGL format
  glBindTexture GL_TEXTURE_CUBE_MAP n
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_X 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_X 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_Y 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_Y 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_Z 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_Z 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  return (Texture n format)
  
-- | Delete a texture.
deleteTexture :: Texture -> IO ()
deleteTexture tex = withArray [texObjectName tex] (\ptr -> glDeleteTextures 1 ptr)

-- | Bind a 2D texture to the 2D texture binding target and the currently
-- active texture unit.
bindTexture2D :: Texture -> IO ()
bindTexture2D = glBindTexture GL_TEXTURE_2D . texObjectName

-- | Bind a cubemap texture to the cubemap texture binding target and
-- the currently active texture unit.
bindTextureCubeMap :: Texture -> IO ()
bindTextureCubeMap = glBindTexture GL_TEXTURE_CUBE_MAP . texObjectName

-- | Set the active texture unit. The default is zero.
setActiveTextureUnit :: Int -> IO ()
setActiveTextureUnit n = glActiveTexture (GL_TEXTURE0 + fromIntegral n)

-- | Set the filtering for the 2D texture currently bound to the 2D texture
-- binding target.
setTex2DFiltering :: Filtering -> IO ()
setTex2DFiltering filt = do
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (toGL filt)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (toGL filt)

-- | Set the filtering for the cubemap texture currently bound to the cubemap
-- texture binding target.
setCubeMapFiltering :: Filtering -> IO ()
setCubeMapFiltering filt = do
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MIN_FILTER (toGL filt)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MAG_FILTER (toGL filt)

-- | Set the wrapping mode for the 2D texture currently bound to the 2D
-- texture binding target.
setTex2DWrapping :: Wrapping -> IO ()
setTex2DWrapping wrap = do
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (toGL wrap)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (toGL wrap)

-- | Set the wrapping mode for the cubemap texture currently bound to the
-- cubemap texture binding target. Because no blending occurs between cube
-- faces you probably want ClampToEdge.
setCubeMapWrapping :: Wrapping -> IO ()
setCubeMapWrapping wrap = do
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_S (toGL wrap)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_T (toGL wrap)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_R (toGL wrap)

-- | Texture filtering modes.
data Filtering =
  Nearest | -- ^ No interpolation.
  Linear    -- ^ Linear interpolation.
    deriving Show

instance ToGL Filtering where
  toGL Nearest = GL_NEAREST
  toGL Linear  = GL_LINEAR

-- | Texture wrapping modes.
data Wrapping =
  Repeat         | -- ^ Tile the texture past the boundary.
  MirroredRepeat | -- ^ Tile the texture but mirror every other tile.
  ClampToEdge      -- ^ Use the edge color for anything past the boundary.
    deriving Show

instance ToGL Wrapping where
  toGL Repeat         = GL_REPEAT
  toGL MirroredRepeat = GL_MIRRORED_REPEAT
  toGL ClampToEdge    = GL_CLAMP_TO_EDGE

-- $example
--
-- <<texture.png Screenshot of Texture Example>>
--
-- This example loads a 256x256 PNG file with JuicyPixels and displays the
-- image on a square. Of course without a correction for aspect ratio the
-- square will only be square if you adjust your window to be square.
--
-- @
-- module Main where
-- 
-- import Control.Monad.Loops (whileM_)
-- import Data.Functor ((\<$\>))
-- import qualified Data.Vector.Storable as V
-- import Codec.Picture
-- import Data.Word
-- 
-- import qualified Graphics.UI.GLFW as GLFW
-- import Linear
-- import Graphics.GL.Low
-- 
-- main = do
--   GLFW.init
--   GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
--   GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
--   GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
--   GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
--   mwin <- GLFW.createWindow 640 480 \"Texture\" Nothing Nothing
--   case mwin of
--     Nothing  -> putStrLn "createWindow failed"
--     Just win -> do
--       GLFW.makeContextCurrent (Just win)
--       GLFW.swapInterval 1
--       (vao, prog, texture) <- setup
--       whileM_ (not \<$\> GLFW.windowShouldClose win) $ do
--         GLFW.pollEvents
--         draw vao prog texture
--         GLFW.swapBuffers win
-- 
-- setup = do
--   -- establish a VAO
--   vao <- newVAO
--   bindVAO vao
--   -- load the shader
--   vsource <- readFile "texture.vert"
--   fsource <- readFile "texture.frag"
--   prog <- newProgram vsource fsource
--   useProgram prog
--   -- load the vertices
--   let blob = V.fromList -- a quad has four vertices
--         [ -0.5, -0.5, 0, 1
--         , -0.5,  0.5, 0, 0
--         ,  0.5, -0.5, 1, 1
--         ,  0.5,  0.5, 1, 0 ] :: V.Vector Float
--   vbo <- newVBO blob StaticDraw
--   bindVBO vbo
--   setVertexLayout [ Attrib "position" 2 GLFloat
--                   , Attrib "texcoord" 2 GLFloat ]
--   -- load the element array to draw a quad with two triangles
--   indices <- newElementArray (V.fromList [0,1,2,3,2,1] :: V.Vector Word8) StaticDraw
--   bindElementArray indices
--   -- load the texture with JuicyPixels
--   let fromRight (Right x) = x
--   ImageRGBA8 (Image w h image) <- fromRight \<$\> readImage "logo.png"
--   texture <- newTexture2D image (w,h) RGBA 
--   setTex2DFiltering Linear
--   return (vao, prog, texture)
-- 
-- draw vao prog texture = do
--   clearColorBuffer (0.5, 0.5, 0.5)
--   bindVAO vao
--   useProgram prog
--   bindTexture2D texture
--   drawIndexedTriangles 6 UByteIndices
-- @
--
-- The vertex shader for this example looks like
--
-- @
-- #version 150
-- in vec2 position;
-- in vec2 texcoord;
-- out vec2 Texcoord;
-- void main()
-- {
--     gl_Position = vec4(position, 0.0, 1.0);
--     Texcoord = texcoord;
-- }
-- @
--
-- And the fragment shader looks like
--
-- @
-- #version 150
-- in vec2 Texcoord;
-- out vec4 outColor;
-- uniform sampler2D tex;
-- void main()
-- {
--   outColor = texture(tex, Texcoord);
-- }
-- @
