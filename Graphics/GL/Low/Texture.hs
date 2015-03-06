{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude hiding (sequence)
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Vector.Storable hiding (mapM_)
import Data.Word
import Control.Applicative
import Data.Traversable (sequence)
import Control.Monad.IO.Class

import Graphics.GL
import Linear

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Internal.Common
import Graphics.GL.Low.Internal.Texture
import Graphics.GL.Low.Classes
import Graphics.GL.Low.Cube


bindNewTexture :: (MonadIO m, Texture a) => m a
bindNewTexture = do
    tex <- genTexture
    bindTexture tex
    return tex

-- | Create a new 2D texture from a blob and its dimensions. Dimensions should
-- be powers of two. The internal format type determines how the data is
-- interpreted.
newTexture2D :: (MonadIO m, Storable a, InternalFormat b)
             => Vector a
             -> Dimensions
             -> m (Tex2D b)
newTexture2D bytes (Dimensions w h) = do
  tex <- bindNewTexture
  texImage2D GL_TEXTURE_2D 0 (internalFormat tex) (V2 w h) (internalFormat tex) GL_UNSIGNED_BYTE (Just bytes)
  glGenerateMipmap GL_TEXTURE_2D
  return tex

-- | Create a new cube map texture from six blobs and their respective dimensions.
-- Dimensions should be powers of two.
newCubeMap :: (MonadIO m, Storable a, InternalFormat b)
           => Cube (Vector a, Dimensions)
           -> m (CubeMap b)
newCubeMap images = do
    cm <- bindNewTexture
    sequence (mkTx cm <$> images <*> cubeSideCodes)
    glGenerateMipmap GL_TEXTURE_CUBE_MAP
    return cm
  where mkTx cm (bytes, Dimensions w h) side = texImage2D side 0 (internalFormat cm) (V2 w h) (internalFormat cm) GL_UNSIGNED_BYTE (Just bytes)

-- | Create an empty texture with the specified dimensions and format.
newEmptyTexture2D :: forall a m. (MonadIO m, Storable a, InternalFormat a) => Int -> Int -> m (Tex2D a)
newEmptyTexture2D w h = do
  tex <- bindNewTexture
  texImage2D GL_TEXTURE_2D 0 (internalFormat tex) (V2 w h) (internalFormat tex) GL_UNSIGNED_BYTE (Nothing :: Maybe (Vector a))
  return tex

-- | Create a cubemap texture where each of the six sides has the specified
-- dimensions and format.
newEmptyCubeMap :: forall a m. (MonadIO m, Storable a, InternalFormat a) => Int -> Int -> m (CubeMap a)
newEmptyCubeMap w h = do
    tex <- bindNewTexture
    mapM_ (mkTx tex) [ GL_TEXTURE_CUBE_MAP_POSITIVE_X
                     , GL_TEXTURE_CUBE_MAP_NEGATIVE_X
                     , GL_TEXTURE_CUBE_MAP_POSITIVE_Y
                     , GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
                     , GL_TEXTURE_CUBE_MAP_POSITIVE_Z
                     , GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
                     ]
    return tex
  where mkTx tex target = texImage2D target 0 (internalFormat tex) (V2 w h) (internalFormat tex) GL_UNSIGNED_BYTE (Nothing :: Maybe (Vector a))


-- | The size of an image in pixels.
data Dimensions = Dimensions
  { imageWidth  :: Int
  , imageHeight :: Int }
    deriving (Show)

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
