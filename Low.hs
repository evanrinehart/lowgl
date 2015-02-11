{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Low (
  -- * VAO
  VAO,
  newVAO,
  bindVAO,

  -- * Buffer Objects
  -- ** VBO
  VBO,
  UsageHint(..),
  newVBO,
  bindVBO,
  updateVBO,

  -- ** Element Array
  ElementArray,
  IndexFormat(..),
  newElementArray,
  bindElementArray,
  updateElementArray,

  -- * Shader Program
  Program,
  ProgramError(..),

  -- ** Compile and Link
  newProgram,
  newProgramSafe,

  -- ** Using
  useProgram,

  -- ** Vertex Attributes
  VertexAttributeLayout(..),
  LayoutElement(..),
  ComponentFormat(..),
  setVertexAttributeLayout,

  -- ** Uniform Variables
  --
  -- | Set uniform variables for the current program. To set an array of
  -- uniforms pass a list of more than one value.

  -- *** Float Uniforms
  -- | These call glUniformNfv.
  setUniform1f, 
  setUniform2f,
  setUniform3f,
  setUniform4f,

  -- *** Int Uniforms
  -- | These call glUniformNiv.
  setUniform1i,
  setUniform2i,
  setUniform3i,
  setUniform4i,

  -- *** Matrix Uniforms
  -- | These call glUniformMatrixNfv.
  setUniform22,
  setUniform33,
  setUniform44,

  -- * Textures
  Tex2D,
  CubeMap,
  ImageFormat(..),
  PixelFormat(..),
  Cube(..),
  newTexture2D,
  newCubeMap,
  newEmptyTexture2D,
  newEmptyCubeMap,
  setActiveTextureUnit,
  bindTexture2D,
  bindTextureCubeMap,
  Filtering(..),
  setTex2DFiltering,
  setCubeMapFiltering,
  Wrapping(..),
  setTex2DWrapping,
  setCubeMapWrapping,

  -- * Rendering

  -- ** Primitives
  -- | Draw primitives to the framebuffer currently bound to the framebuffer
  -- binding target. Each primitive drawing command takes the number of vertices
  -- in the VBOs to render. The vertices are traversed in order.
  drawPoints,
  drawLines,
  drawLineStrip,
  drawLineLoop,
  drawTriangles,
  drawTriangleStrip,
  drawTriangleFan,

  -- ** Primitives by Index
  -- | Draw primitives as above, but use the order of vertices defined in
  -- the ElementArray currently bound to the element array buffer binding
  -- target.
  drawIndexedPoints,
  drawIndexedLines,
  drawIndexedLineStrip,
  drawIndexedLineLoop,
  drawIndexedTriangles,
  drawIndexedTriangleStrip,
  drawIndexedTriangleFan,

  -- ** Color Buffer
  enableColorWriting,
  disableColorWriting,
  Color(..),
  clearColorBuffer,

  -- ** Depth Test
  enableDepthTest,
  disableDepthTest,
  clearDepthBuffer,
  enableDepthWriting,
  disableDepthWriting,

  -- ** Stencil Test
  enableStencilTest,
  disableStencilTest,
  clearStencilBuffer,
  enableStencilWriting,
  disableStencilWriting,

  -- ** Scissor Test
  setScissorBox,
  enableScissorTest,
  disableScissorTest,

  -- ** Facet Culling
  Culling(..),
  enableCulling,
  disableCulling,

  -- ** Viewport
  Viewport(..),
  setViewport,

  -- * Framebuffers
  FBO,
  defaultFBO,
  newFramebuffer,
  bindFramebuffer,
  attachTex2DColor,
  attachCubeLeftColor,
  attachCubeRightColor,
  attachCubeTopColor,
  attachCubeBottomColor,
  attachCubeFrontColor,
  attachCubeBackColor,
  attachRBOColor,
  attachRBODepth,
  attachRBODepthStencil,

  -- * Renderbuffers
  RBOColor,
  RBODepth,
  RBODepthStencil,
  newRBOColor,
  newRBODepth,
  newRBODepthStencil

) where

import Control.Exception
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.String
import Data.Vector.Storable (Vector, unsafeWith)
import qualified Data.Vector.Storable as V (length)
import Control.Monad
import Data.Word
import Data.Int
import Data.Functor

import Linear
import Graphics.GL

-- | A VAO stores vertex attribute layouts and the VBO source of vertices
-- for those attributes. It also stores the state of the element array binding
-- target. The vertex array binding target admits one VAO at a time.
newtype VAO = VAO GLuint deriving Show

-- | A Program object is the combination of a compiled vertex shader and fragment
-- shader. Programs have three kinds of inputs: vertex attributes, uniforms,
-- and samplers. Programs have two outputs: fragment color and fragment depth.
-- At most one program can be "in use" at a time. Same idea as binding targets
-- it's just not called that.
newtype Program = Program GLuint deriving Show

-- | A VBO is a buffer object which has vertex data. Programs use VBOs as
-- input to their vertex attributes according to the configuration of the
-- bound VAO.
data VBO = VBO GLuint Int deriving Show

-- | A buffer object which has a sequence of vertex indices. Indexed rendering
-- uses the ElementArray bound to the element array binding target.
data ElementArray = ElementArray GLuint Int IndexFormat deriving Show

-- | A 2D texture. A program can sample a texture if it has been bound to
-- the appropriate texture unit.
newtype Tex2D = Tex2D GLuint deriving Show

-- | A cubemap texture is just six 2D textures. A program can sample a cubemap
-- texture if it has been bound to the appropriate texture unit.
newtype CubeMap = CubeMap GLuint deriving Show

-- | A framebuffer object contains up to three attachments: a color buffer,
-- a depth buffer, and a stencil buffer. Each attachment may be a texture
-- object or a renderbuffer object. Rendering commands output graphics to the
-- attachments of the FBO currently bound to the framebuffer binding target.
newtype FBO = FBO GLuint deriving Show

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

-- | The name of a vertex input to a program combined with the
-- component format and number of components for that attribute in the
-- vertex data. Alternatively the size of an unused section of the data
-- in bytes.
data LayoutElement =
  Attrib String Int ComponentFormat | -- ^ Name, component count and component format of a vertex attribute.
  Unused Int -- ^ Size in bytes of an unused section of the vertex data.
    deriving Show

-- | The layout of interleaved vertex attribute data.
type VertexAttributeLayout = [LayoutElement]

-- | The size and interpretation of a vertex attribute component. Normalized
-- components will be mapped to floats in the range [0, 1]. Unnormalized
-- integral components will be mapped to ints in the shader program.
data ComponentFormat =
  VFloat | -- ^ 4-byte float
  VByte |
  VUByte | 
  VByteNormalized | 
  VUByteNormalized |
  VShort | -- ^ 2-byte signed integer
  VUShort | -- ^ 2-byte unsigned integer
  VShortNormalized |
  VUShortNormalized |
  VInt | -- ^ 4-byte signed integer
  VUInt | -- ^ 4-byte unsigned integer
  VIntNormalized |
  VUIntNormalized
    deriving (Eq, Show)

instance ToGL ComponentFormat where
  toGL VFloat = GL_FLOAT
  toGL VByte = GL_BYTE
  toGL VUByte = GL_UNSIGNED_BYTE
  toGL VByteNormalized = GL_BYTE
  toGL VUByteNormalized = GL_UNSIGNED_BYTE
  toGL VShort = GL_SHORT
  toGL VUShort = GL_UNSIGNED_SHORT
  toGL VShortNormalized = GL_SHORT
  toGL VUShortNormalized = GL_UNSIGNED_SHORT
  toGL VInt = GL_INT
  toGL VUInt = GL_UNSIGNED_INT
  toGL VIntNormalized = GL_INT
  toGL VUIntNormalized = GL_UNSIGNED_INT
  

-- | How indices are packed in an ElementArray buffer object.
data IndexFormat =
  UByteIndices  |  -- ^ Each index is one unsigned byte.
  UShortIndices |  -- ^ Each index is a two byte unsigned int.
  UIntIndices      -- ^ Each index is a four byte unsigned int.
    deriving Show

instance ToGL IndexFormat where
  toGL UByteIndices  = GL_UNSIGNED_BYTE
  toGL UShortIndices = GL_UNSIGNED_SHORT
  toGL UIntIndices   = GL_UNSIGNED_INT

data UsageHint = StaticDraw  -- ^ Data will seldomly change.
               | DynamicDraw -- ^ Data will change.
               | StreamDraw  -- ^ Data will change very often.
                 deriving Show

instance ToGL UsageHint where
  toGL StreamDraw  = GL_STREAM_DRAW
  toGL StaticDraw  = GL_STATIC_DRAW
  toGL DynamicDraw = GL_DYNAMIC_DRAW

data Color = Color !Float !Float !Float !Float deriving Show

data PixelFormat =
  Alpha          | -- ^ 1-byte alpha channel only.
  Luminance      | -- ^ 1-byte grayscale pixels.
  LuminanceAlpha | -- ^ 2-byte luminance and alpha channel.
  RGB            | -- ^ 3-byte true color pixels.
  RGBA             -- ^ 4-byte true color pixels with alpha channel.
    deriving (Eq, Show)

instance ToGL PixelFormat where
  toGL Alpha = GL_ALPHA
  toGL Luminance = GL_LUMINANCE
  toGL LuminanceAlpha = GL_LUMINANCE_ALPHA
  toGL RGB = GL_RGB
  toGL RGBA = GL_RGBA

-- | A section of the window.
data Viewport = Viewport
  { viewportX :: Int
  , viewportY :: Int
  , viewportW :: Int
  , viewportH :: Int }
    deriving (Eq, Show)

-- | The size of an image in pixels and the format of the packed pixels.
data ImageFormat = ImageFormat
  { imageWidth :: Int
  , imageHeight :: Int
  , imageFormat :: PixelFormat }
    deriving (Show)

-- | Six values.
data Cube a = Cube
  { cubeRight  :: a
  , cubeLeft   :: a
  , cubeTop    :: a
  , cubeBottom :: a
  , cubeFront  :: a
  , cubeBack   :: a } deriving Show

instance Functor Cube where
  fmap f (Cube x y u v s t) = Cube (f x) (f y) (f u) (f v) (f s) (f y)

data ShaderType = VertexShader | FragmentShader deriving Show

instance ToGL ShaderType where
  toGL VertexShader = GL_VERTEX_SHADER
  toGL FragmentShader = GL_FRAGMENT_SHADER

-- | The error message emitted by the driver when shader compilation or
-- linkage fails.
data ProgramError =
  VertexShaderError String |
  FragmentShaderError String |
  LinkError String
    deriving (Show, Typeable)
  
instance Exception ProgramError

class ToGL a where
  toGL :: (Num b, Eq b) => a -> b

-- | Create a new VAO. The only thing you can do with a VAO is bind it to
-- the vertex array binding target.
newVAO :: IO VAO
newVAO = do
  n <- alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)
  return (VAO n)

-- | Assign the VAO to the vertex array binding target. The VAO already bound
-- will be replaced, if any.
bindVAO :: VAO -> IO ()
bindVAO (VAO n) = glBindVertexArray n


-- | Create a buffer object from a blob of bytes. The usage argument hints
-- at how often you will modify the data.
newVBO :: Vector Word8 -> UsageHint -> IO VBO
newVBO src usage = do
  n <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  let len = V.length src
  glBindBuffer GL_ARRAY_BUFFER n
  unsafeWith src $ \ptr -> glBufferData
    GL_ARRAY_BUFFER
    (fromIntegral len)
    (castPtr ptr)
    (toGL usage)
  return (VBO n len)

-- | Modify the data in the currently bound VBO starting from the specified
-- index in bytes.
updateVBO :: Vector Word8 -> Int -> IO ()
updateVBO src offset = do
  let len = V.length src
  unsafeWith src $ \ptr -> glBufferSubData
    GL_ARRAY_BUFFER 
    (fromIntegral offset)
    (fromIntegral len)
    (castPtr ptr)

bindVBO :: VBO -> IO ()
bindVBO (VBO n _) = glBindBuffer GL_ARRAY_BUFFER n



-- | Pack a list of indices into a new buffer object. The usage argument
-- hints at how often you plan to modify the data.
newElementArray :: [Int] -> IndexFormat -> UsageHint -> IO ElementArray
newElementArray xs fmt usage = do
  n <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER n
  len <- marshalIndexes xs fmt $ \len ptr -> do
    glBufferData
      GL_ELEMENT_ARRAY_BUFFER
      (fromIntegral len)
      (castPtr ptr)
      (toGL usage)
    return len
  return (ElementArray n len fmt)
  
-- | Modify contents in the currently bound ElementArray starting at the
-- specified index in bytes.
updateElementArray :: [Int] -> IndexFormat -> Int -> IO ()
updateElementArray xs fmt offset = marshalIndexes xs fmt $ \len ptr -> do
  glBufferSubData
    GL_ELEMENT_ARRAY_BUFFER
    (fromIntegral offset)
    (fromIntegral len)
    (castPtr ptr)

-- | internal
marshalIndexes :: [Int] -> IndexFormat -> (Int -> Ptr Word8 -> IO a) -> IO a
marshalIndexes xs fmt act = case fmt of
  UByteIndices  -> withArrayLen (map fromIntegral xs :: [Word8]) act
  UShortIndices -> withArrayLen (map fromIntegral xs :: [Word16])
                     (\n ptr -> act n (castPtr ptr))
  UIntIndices   -> withArrayLen (map fromIntegral xs :: [Word32])
                     (\n ptr -> act n (castPtr ptr))


-- | Assign an ElementArray to the element array binding target. It will
-- replace the ElementArray already bound there, if any. Note that the state
-- of the element array binding target is a function of the current VAO.
bindElementArray :: ElementArray -> IO ()
bindElementArray (ElementArray n _ _) = glBindBuffer GL_ELEMENT_ARRAY_BUFFER n


-- | Same as 'newProgram' but does not throw exceptions.
newProgramSafe :: String -> String -> IO (Either ProgramError Program)
newProgramSafe vcode fcode = try $ newProgram vcode fcode

-- | Compile the code for a vertex shader and a fragment shader, then link
-- them into a new program. If the compiler or linker fails it will throw
-- a ProgramError.
newProgram :: String -- ^ vertex shader source code
           -> String -- ^ fragment shader source code
           -> IO Program
newProgram vcode fcode = do
  vertexShaderId <- compileShader vcode VertexShader
  fragmentShaderId <- compileShader fcode FragmentShader
  programId <- glCreateProgram
  glAttachShader programId vertexShaderId
  glAttachShader programId fragmentShaderId
  glLinkProgram programId
  result <- alloca $ \ptr ->
    glGetProgramiv programId GL_LINK_STATUS ptr >> peek ptr
  when (result == GL_FALSE) $ do
    len <- fmap fromIntegral $ alloca $ \ptr ->
      glGetProgramiv programId GL_INFO_LOG_LENGTH ptr >> peek ptr
    errors <- allocaArray len $ \ptr -> do
      glGetProgramInfoLog programId (fromIntegral len) nullPtr ptr
      peekCString ptr
    throwIO (LinkError errors)
  glDeleteShader vertexShaderId
  glDeleteShader fragmentShaderId
  return (Program programId)

-- | Install a program into the rendering pipeline. Replaces the program
-- already in use, if any.
useProgram :: Program -> IO ()
useProgram (Program n) = glUseProgram n

-- | internal
compileShader :: String -> ShaderType -> IO GLuint
compileShader code vertOrFrag = do
  shaderId <- glCreateShader (toGL vertOrFrag)
  withCString code $ \ptr -> with ptr $ \pptr -> do
    glShaderSource shaderId 1 pptr nullPtr
    glCompileShader shaderId
  result <- with GL_FALSE $ \ptr ->
    glGetShaderiv shaderId GL_COMPILE_STATUS ptr >> peek ptr
  when (result == GL_FALSE) $ do
    len <- fmap fromIntegral $ alloca $ \ptr ->
      glGetShaderiv shaderId GL_INFO_LOG_LENGTH ptr >> peek ptr
    errors <- allocaArray len $ \ptr -> do
      glGetShaderInfoLog shaderId (fromIntegral len) nullPtr ptr
      peekCString ptr
    case vertOrFrag of
      VertexShader -> throwIO (VertexShaderError errors)
      FragmentShader -> throwIO (FragmentShaderError errors)
  return shaderId


-- | This configures the currently bound VAO. It calls glVertexAttribPointer
-- and glEnableVertexAttribArray.
setVertexAttributeLayout :: Program -> VertexAttributeLayout -> IO ()
setVertexAttributeLayout (Program p) layout = do
  let layout' = elaborateLayout 0 layout
  let total = totalLayout layout
  forM_ layout' $ \(name, size, offset, fmt) -> do
    attrib <- withCString name $ \ptr -> glGetAttribLocation p (castPtr ptr)
    let norm = isNormalized fmt
    glVertexAttribPointer
      (fromIntegral attrib)
      (fromIntegral size)
      (toGL fmt)
      (fromIntegral . fromEnum $ norm)
      (fromIntegral offset)
      (castPtr (nullPtr `plusPtr` offset))
    glEnableVertexAttribArray (fromIntegral attrib)

elaborateLayout :: Int -> VertexAttributeLayout -> [(String, Int, Int, ComponentFormat)]
elaborateLayout here layout = case layout of
  [] -> []
  (Unused n):xs -> elaborateLayout (here+n) xs
  (Attrib name n fmt):xs ->
    let size = n * sizeOfVertexComponent fmt in
    (name, n, here, fmt) : elaborateLayout (here+size) xs

totalLayout :: VertexAttributeLayout -> Int
totalLayout layout = sum (map arraySize layout) where
  arraySize (Unused n) = n
  arraySize (Attrib _ n fmt) = n * sizeOfVertexComponent fmt

sizeOfVertexComponent :: ComponentFormat -> Int
sizeOfVertexComponent c = case c of
  VByte -> 1
  VUByte -> 1
  VByteNormalized -> 1
  VUByteNormalized -> 1
  VShort -> 2
  VUShort -> 2
  VShortNormalized -> 2
  VUShortNormalized -> 2
  VInt -> 4
  VUInt -> 4
  VIntNormalized -> 4
  VUIntNormalized -> 4
  VFloat -> 4

isNormalized :: ComponentFormat -> Bool
isNormalized c = case c of
  VByte -> False
  VUByte -> False
  VByteNormalized -> True
  VUByteNormalized -> True
  VShort -> False
  VUShort -> False
  VShortNormalized -> True
  VUShortNormalized -> True
  VInt -> False
  VUInt -> False
  VIntNormalized -> True
  VUIntNormalized -> True
  VFloat -> False



setUniform1f :: Program -> String -> [Float] -> IO ()
setUniform1f = setUniform glUniform1fv

setUniform2f :: Program -> String -> [V2 Float] -> IO ()
setUniform2f = setUniform
  (\loc cnt val -> glUniform2fv loc cnt (castPtr val))

setUniform3f :: Program -> String -> [V3 Float] -> IO ()
setUniform3f = setUniform
  (\loc cnt val -> glUniform3fv loc cnt (castPtr val))

setUniform4f :: Program -> String -> [V4 Float] -> IO ()
setUniform4f = setUniform
  (\loc cnt val -> glUniform4fv loc cnt (castPtr val))

setUniform1i :: Program -> String -> [Int] -> IO ()
setUniform1i = setUniform
  (\loc cnt val -> glUniform1iv loc cnt (castPtr val))

setUniform2i :: Program -> String -> [V2 Int] -> IO ()
setUniform2i = setUniform 
  (\loc cnt val -> glUniform2iv loc cnt (castPtr val))

setUniform3i :: Program -> String -> [V3 Int] -> IO ()
setUniform3i = setUniform
  (\loc cnt val -> glUniform3iv loc cnt (castPtr val))

setUniform4i :: Program -> String -> [V4 Int] -> IO ()
setUniform4i = setUniform
  (\loc cnt val -> glUniform4iv loc cnt (castPtr val))

setUniform44 :: Program -> String -> [M44 Float] -> IO ()
setUniform44 = setUniform
  (\loc cnt val -> glUniformMatrix4fv loc cnt GL_FALSE (castPtr val))

setUniform33 :: Program -> String -> [M33 Float] -> IO ()
setUniform33 = setUniform
  (\loc cnt val -> glUniformMatrix3fv loc cnt GL_FALSE (castPtr val))

setUniform22 :: Program -> String -> [M22 Float] -> IO ()
setUniform22 = setUniform
  (\loc cnt val -> glUniformMatrix2fv loc cnt GL_FALSE (castPtr val))

setUniform :: Storable a => (GLint -> GLsizei -> Ptr a -> IO ())
           -> Program -> String -> [a]
           -> IO ()
setUniform glAction (Program p) name xs = withArrayLen xs $ \n bytes -> do
  loc <- withCString name (\ptr -> glGetUniformLocation p ptr)
  glAction loc (fromIntegral n) bytes
  

  

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

-- | Create a new 2D texture from a blob, given its dimensions and pixel format.
-- Dimensions should be powers of two.
newTexture2D :: Vector Word8 -> ImageFormat -> IO Tex2D
newTexture2D bytes (ImageFormat w h pixfmt)  = do
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_2D n
  unsafeWith bytes $ \ptr -> glTexImage2D
    GL_TEXTURE_2D
    0
    (toGL pixfmt)
    (fromIntegral w)
    (fromIntegral h)
    0
    (toGL pixfmt)
    GL_UNSIGNED_BYTE
    (castPtr ptr)
  return (Tex2D n)

-- | Create a new cube map texture from six blobs and their respective formats.
-- Dimensions should be powers of two.
newCubeMap :: Cube (Vector Word8, ImageFormat) -> IO CubeMap
newCubeMap (Cube s1 s2 s3 s4 s5 s6) = do
  n <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_CUBE_MAP n
  loadCubeMapSide s1 GL_TEXTURE_CUBE_MAP_POSITIVE_X
  loadCubeMapSide s2 GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  loadCubeMapSide s3 GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  loadCubeMapSide s4 GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  loadCubeMapSide s5 GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  loadCubeMapSide s6 GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
  return (CubeMap n)
  
loadCubeMapSide :: (Vector Word8, ImageFormat) -> GLenum -> IO ()
loadCubeMapSide (bytes, ImageFormat w h pixfmt) side = do
  unsafeWith bytes $ \ptr -> glTexImage2D
    side
    0
    (toGL pixfmt)
    (fromIntegral w)
    (fromIntegral h)
    0
    (toGL pixfmt)
    GL_UNSIGNED_BYTE
    (castPtr ptr)

-- | Create an empty texture with the specified dimensions and format.
newEmptyTexture2D :: Int -> Int -> PixelFormat -> IO Tex2D
newEmptyTexture2D w h fmt = do
  let w' = fromIntegral w
  let h' = fromIntegral h
  let fmt' = toGL fmt
  let fmt'' = toGL fmt
  tex <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_2D tex
  glTexImage2D GL_TEXTURE_2D 0 fmt' w' h' 0 fmt'' GL_UNSIGNED_BYTE nullPtr
  return (Tex2D tex)

-- | Create a cubemap texture where each of the six sides has the specified
-- dimensions and format.
newEmptyCubeMap :: Int -> Int -> PixelFormat -> IO CubeMap
newEmptyCubeMap w h fmt = do
  let w' = fromIntegral w
  let h' = fromIntegral h
  let fmt' = toGL fmt
  let fmt'' = toGL fmt
  tex <- alloca (\ptr -> glGenTextures 1 ptr >> peek ptr)
  glBindTexture GL_TEXTURE_CUBE_MAP tex
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_X 0 fmt' w' h' 0 fmt'' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_X 0 fmt' w' h' 0 fmt'' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_Y 0 fmt' w' h' 0 fmt'' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_Y 0 fmt' w' h' 0 fmt'' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_POSITIVE_Z 0 fmt' w' h' 0 fmt'' GL_UNSIGNED_BYTE nullPtr
  glTexImage2D GL_TEXTURE_CUBE_MAP_NEGATIVE_Z 0 fmt' w' h' 0 fmt'' GL_UNSIGNED_BYTE nullPtr
  return (CubeMap tex)
  

-- | Bind a 2D texture to the 2D texture binding target and the currently
-- active texture unit.
bindTexture2D :: Tex2D -> IO ()
bindTexture2D (Tex2D n) = glBindTexture GL_TEXTURE_2D n

-- | Bind a cubemap texture to the cubemap texture binding target and
-- the currently active texture unit.
bindTextureCubeMap :: CubeMap -> IO ()
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

  

-- | Allow rendering commands to modify the color buffer of the current
-- framebuffer.
enableColorWriting :: IO ()
enableColorWriting = glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE

disableColorWriting :: IO ()
disableColorWriting = glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE

-- | Clear the color buffer of the current framebuffer with the specified
-- color. Has no effect if writing to the color buffer is disabled.
clearColorBuffer :: Color -> IO ()
clearColorBuffer (Color r g b a) = do
  glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
  glClear GL_COLOR_BUFFER_BIT

-- | Enable the depth test. Attempting to render pixels with a depth value
-- greater than the depth buffer at those pixels will have no effect.
enableDepthTest :: IO ()
enableDepthTest = glEnable GL_DEPTH_TEST

-- | Disable the depth test. Rendering will not be affected by the depth.
-- Use this to render graphics even if they are behind something.
disableDepthTest :: IO ()
disableDepthTest = glDisable GL_DEPTH_TEST

-- | Enable writing depth values to the depth buffer of the current framebuffer.
-- It is enabled by default.
enableDepthWriting :: IO ()
enableDepthWriting = glDepthMask GL_TRUE

-- | Disable writing to the depth buffer.
disableDepthWriting :: IO ()
disableDepthWriting = glDepthMask GL_FALSE

-- | Clear the depth buffer with the maximum depth value.
clearDepthBuffer :: IO ()
clearDepthBuffer = glClear GL_DEPTH_BUFFER_BIT


enableStencilTest :: IO ()
enableStencilTest = do
  glStencilFunc GL_LESS 1 maxBound
  glStencilOp GL_KEEP GL_KEEP GL_KEEP
  glEnable GL_STENCIL_TEST

-- | Disable the stencil test.
disableStencilTest :: IO ()
disableStencilTest = glDisable GL_STENCIL_TEST

-- | Clear the stencil buffer with all zeros.
clearStencilBuffer :: IO ()
clearStencilBuffer = glClear GL_STENCIL_BUFFER_BIT

enableStencilWriting :: IO ()
enableStencilWriting = do
  glStencilFunc GL_ALWAYS 1 maxBound
  glStencilOp GL_KEEP GL_KEEP GL_REPLACE
  glStencilMask 1

disableStencilWriting :: IO ()
disableStencilWriting = glStencilMask 0



-- | Set the scissor box. Graphics outside this box will not be rendered as
-- long as the scissor test is enabled.
setScissorBox :: Viewport -> IO ()
setScissorBox (Viewport x y w h) =
  glScissor (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

-- | Enable the scissor test. Graphics outside the scissor box will not be
-- rendered.
enableScissorTest :: IO ()
enableScissorTest = glEnable GL_SCISSOR_TEST

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

-- viewport
setViewport :: Viewport -> IO ()
setViewport (Viewport x y w h) =
  glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

-- | Create a framebuffer object. Before the framebuffer can be used for
-- rendering it must have a color attachment texture and that texture must
-- have been initialized with storage.
newFramebuffer :: IO FBO
newFramebuffer = do
  n <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
  return (FBO n)

data RBOColor = RBOColor GLuint deriving Show
data RBODepth = RBODepth GLuint deriving Show
data RBODepthStencil = RBODepthStencil GLuint deriving Show


attachRBOColor :: RBOColor -> IO ()
attachRBOColor (RBOColor n) = glFramebufferRenderbuffer
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_RENDERBUFFER n

attachRBODepth :: RBODepth -> IO ()
attachRBODepth (RBODepth n) = glFramebufferRenderbuffer
  GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER n

attachRBODepthStencil :: RBODepthStencil -> IO ()
attachRBODepthStencil (RBODepthStencil n) = glFramebufferRenderbuffer
  GL_FRAMEBUFFER GL_DEPTH_STENCIL_ATTACHMENT GL_RENDERBUFFER n

attachTex2DColor :: Tex2D -> IO ()
attachTex2DColor (Tex2D n) = glFramebufferTexture2D
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D n 0


attachCubeLeftColor :: CubeMap -> IO ()
attachCubeLeftColor (CubeMap n) = glFramebufferTexture2D
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_CUBE_MAP_NEGATIVE_X n 0

attachCubeRightColor :: CubeMap -> IO ()
attachCubeRightColor (CubeMap n) = glFramebufferTexture2D
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_CUBE_MAP_POSITIVE_X n 0

attachCubeTopColor :: CubeMap -> IO ()
attachCubeTopColor (CubeMap n) = glFramebufferTexture2D
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_CUBE_MAP_POSITIVE_Y n 0

attachCubeBottomColor :: CubeMap -> IO ()
attachCubeBottomColor (CubeMap n) = glFramebufferTexture2D
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_CUBE_MAP_NEGATIVE_Y n 0

attachCubeFrontColor :: CubeMap -> IO ()
attachCubeFrontColor (CubeMap n) = glFramebufferTexture2D
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_CUBE_MAP_POSITIVE_Z n 0

attachCubeBackColor :: CubeMap -> IO ()
attachCubeBackColor (CubeMap n) = glFramebufferTexture2D
  GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_CUBE_MAP_NEGATIVE_Z n 0

-- | Have rendering commands output to the desired framebuffer. Assigns
-- framebuffer object to framebuffer binding target.
bindFramebuffer :: FBO -> IO ()
bindFramebuffer (FBO n) = glBindFramebuffer GL_FRAMEBUFFER n

-- | The default framebuffer object. Bind this to render normally.
defaultFBO :: FBO
defaultFBO = FBO 0

-- | Create a new 24-bit color renderbuffer with the specified dimensions.
newRBOColor :: Int -> Int -> IO RBOColor
newRBOColor w h = RBOColor <$> newRBO GL_RGB8 w h

-- | Create a new renderbuffer with 24-bit depth component with the specified
-- dimensions.
newRBODepth :: Int -> Int -> IO RBODepth
newRBODepth w h = RBODepth <$> newRBO GL_DEPTH_COMPONENT24 w h

-- | Create a new renderbuffer with 24-bit depth component and 8-bit stencil
-- with the specified dimensions.
newRBODepthStencil :: Int -> Int -> IO RBODepthStencil
newRBODepthStencil w h = RBODepthStencil <$> newRBO GL_DEPTH24_STENCIL8 w h

newRBO :: GLenum -> Int -> Int -> IO GLuint
newRBO internal w h = do
  n <- alloca (\ptr -> glGenRenderbuffers 1 ptr >> peek ptr)
  glBindRenderbuffer GL_RENDERBUFFER n
  glRenderbufferStorage
    GL_RENDERBUFFER
    internal
    (fromIntegral w)
    (fromIntegral h)
  return n

