module Graphics.GL.Low.Texture (

-- | Textures are objects that contain image data that can be sampled by
-- a shader. While an obvious application of this is texture mapping, there
-- are many other uses for textures (the image data doesn't have to be an
-- image at all, it can represent anything).
--
-- Each sampler uniform in your shader points to a texture unit (zero by
-- default). This texture unit is where it will read texture data from. To
-- assign a texture to a texture unit, use 'setActiveTextureUnit' then bind
-- a texture. This will not only bind it to the relevant texture binding target
-- but also to the active texture unit. You can change which unit a sampler
-- points to by setting it using the 'Graphics.GL.Low.Shader.setUniform1i'
-- command. You can avoid dealing with active texture units if theres only one
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
  Tex2D,
  CubeMap,
  Filtering(..),
  Wrapping(..),
  Dimensions(..)

  -- * Example
  -- $example

) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Vector.Storable
import Data.Word
import Control.Applicative
import Data.Traversable (sequenceA)

import Graphics.GL

import Graphics.GL.Low.Classes
import Graphics.GL.Low.Common
import Graphics.GL.Low.Cube


-- | Create a new 2D texture from a blob and its dimensions. Dimensions should
-- be powers of two. The internal format type determines how the data is
-- interpreted.
newTexture2D :: (Storable a, InternalFormat b)
             => Vector a
             -> Dimensions
             -> IO (Tex2D b)
newTexture2D bytes (Dimensions w h)  = do
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_2D n
  tex <- return (Tex2D n)
  unsafeWith bytes $ \ptr -> glTexImage2D
    GL_TEXTURE_2D
    0
    (internalFormat tex)
    (fromIntegral w)
    (fromIntegral h)
    0
    (internalFormat tex)
    GL_UNSIGNED_BYTE
    (castPtr ptr)
  glGenerateMipmap GL_TEXTURE_2D
  return tex

-- | Create a new cube map texture from six blobs and their respective dimensions.
-- Dimensions should be powers of two.
newCubeMap :: (Storable a, InternalFormat b)
           => Cube (Vector a, Dimensions)
           -> IO (CubeMap b)
newCubeMap images = do
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_CUBE_MAP n
  cm <- return (CubeMap n)
  let fmt = internalFormat cm
  sequenceA (liftA2 (loadCubeMapSide fmt) images cubeSideCodes)
  glGenerateMipmap GL_TEXTURE_CUBE_MAP
  return cm

  
loadCubeMapSide :: Storable a => GLenum -> (Vector a, Dimensions) -> GLenum -> IO ()
loadCubeMapSide fmt (bytes, (Dimensions w h)) side = do
  unsafeWith bytes $ \ptr -> glTexImage2D
    side
    0
    (fromIntegral fmt)
    (fromIntegral w)
    (fromIntegral h)
    0
    fmt
    GL_UNSIGNED_BYTE
    (castPtr ptr)

-- | Create an empty texture with the specified dimensions and format.
newEmptyTexture2D :: InternalFormat a => Int -> Int -> IO (Tex2D a)
newEmptyTexture2D w h = do
  let w' = fromIntegral w
  let h' = fromIntegral h
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  tex <- return (Tex2D n)
  let fmt = internalFormat tex
  let fmt' = internalFormat tex
  glBindTexture GL_TEXTURE_2D n
  glTexImage2D GL_TEXTURE_2D 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  return tex

-- | Create a cubemap texture where each of the six sides has the specified
-- dimensions and format.
newEmptyCubeMap :: InternalFormat a => Int -> Int -> IO (CubeMap a)
newEmptyCubeMap w h = do
  let w' = fromIntegral w
  let h' = fromIntegral h
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  tex <- return (CubeMap n)
  let fmt = internalFormat tex
  let fmt' = internalFormat tex
  glBindTexture GL_TEXTURE_CUBE_MAP n
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_X 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_X 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_Y 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_Y 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_Z 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_Z 0 fmt w' h' 0 fmt' GL_UNSIGNED_BYTE nullPtr
  return tex
  
-- | Delete a texture.
deleteTexture :: Texture a => a -> IO ()
deleteTexture x = withArray [glObjectName x] (\ptr -> glDeleteTextures 1 ptr)

-- | Bind a 2D texture to the 2D texture binding target and the currently
-- active texture unit.
bindTexture2D :: Tex2D a -> IO ()
bindTexture2D (Tex2D n) = glBindTexture GL_TEXTURE_2D n

-- | Bind a cubemap texture to the cubemap texture binding target and
-- the currently active texture unit.
bindTextureCubeMap :: CubeMap a -> IO ()
bindTextureCubeMap (CubeMap n) = glBindTexture GL_TEXTURE_CUBE_MAP n

-- | Set the active texture unit. The default is zero.
setActiveTextureUnit :: Enum a => a -> IO ()
setActiveTextureUnit n =
  (glActiveTexture . fromIntegral) (GL_TEXTURE0 + fromEnum n)

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

-- | A 2D texture. A program can sample a texture if it has been bound to
-- the appropriate texture unit.
newtype Tex2D a = Tex2D GLuint deriving Show

-- | A cubemap texture is just six 2D textures. A program can sample a cubemap
-- texture if it has been bound to the appropriate texture unit.
newtype CubeMap a = CubeMap GLuint deriving Show

-- | Texture filtering modes.
data Filtering =
  Nearest | -- ^ No interpolation.
  Linear    -- ^ Linear interpolation.
    deriving Show

instance ToGL Filtering where
  toGL Nearest = GL_NEAREST
  toGL Linear = GL_LINEAR

-- | Texture wrapping modes.
data Wrapping =
  Repeat         | -- ^ Tile the texture past the boundary.
  MirroredRepeat | -- ^ Tile the texture but mirror every other tile.
  ClampToEdge      -- ^ Use the edge color for anything past the boundary.
    deriving Show

instance ToGL Wrapping where
  toGL Repeat = GL_REPEAT
  toGL MirroredRepeat = GL_MIRRORED_REPEAT
  toGL ClampToEdge = GL_CLAMP_TO_EDGE

-- | The size of an image in pixels.
data Dimensions = Dimensions
  { imageWidth  :: Int
  , imageHeight :: Int }
    deriving (Show)

instance Texture (Tex2D a) where

instance Texture (CubeMap a) where

instance GLObject (Tex2D a) where
  glObjectName (Tex2D n) = fromIntegral n

instance GLObject (CubeMap a) where
  glObjectName (CubeMap n) = fromIntegral n

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
--   texture <- newTexture2D image (Dimensions w h) :: IO (Tex2D RGBA)
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
