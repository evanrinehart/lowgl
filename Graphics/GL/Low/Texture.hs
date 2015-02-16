module Graphics.GL.Low.Texture (
  Tex2D,
  CubeMap,
  Filtering(..),
  Wrapping(..),
  Dimensions(..),
  newTexture2D,
  deleteTexture,
  newCubeMap,
  newEmptyTexture2D,
  newEmptyCubeMap,
  bindTexture2D,
  bindTextureCubeMap,
  setActiveTextureUnit,
  setTex2DFiltering,
  setCubeMapFiltering,
  setTex2DWrapping,
  setCubeMapWrapping
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

-- | Create a new 2D texture from a blob and its image format.
-- Dimensions should be powers of two.
newTexture2D :: InternalFormat a => Vector Word8 -> Dimensions -> IO (Tex2D a)
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
  return tex

-- | Delete a texture.
deleteTexture :: Texture a => a -> IO ()
deleteTexture x = withArray [glObjectName x] (\ptr -> glDeleteTextures 1 ptr)

-- | Create a new cube map texture from six blobs and their respective formats.
-- Dimensions should be powers of two.
newCubeMap :: InternalFormat a
           => Cube (Vector Word8, Dimensions)
           -> IO (CubeMap a)
newCubeMap images = do
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_CUBE_MAP n
  cm <- return (CubeMap n)
  let fmt = internalFormat cm
  sequenceA (liftA2 (loadCubeMapSide fmt) images cubeSideCodes)
  return cm
  
loadCubeMapSide :: GLenum -> (Vector Word8, Dimensions) -> GLenum -> IO ()
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
