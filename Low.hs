{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.GL.Low (

  -- * Overview
  -- | OpenGL is a graphics rendering interface. This library exposes a vastly
  -- simplified subset of OpenGL that is hopefully still complete enough for
  -- many purposes, such as following tutorials, making simple games, and
  -- demos. In particular the intention is to concentrate on a subset of
  -- OpenGL 3.2 (Core Profile) roughly corresponding to ES 2.0.
  --
  -- A second primary purpose is to document the complex model behind the
  -- interface in a way that is more elaborate than tutorials and more concise
  -- than the spec. As such, this is an experimental project to aid my own
  -- process of understanding OpenGL. It seems that understanding the entire
  -- picture up-front is the only way to get started, so this should also serve
  -- as a quick reference guide to the core commands and concepts.
  --
  -- This library uses the `gl' package for raw bindings to OpenGL and the
  -- `linear' package for matrices.
  --
  -- (include link to example programs)

  -- * Objects
  -- | Objects may be created and destroyed by client code. They include:
  --
  -- - Vertex Array Object ('VAO')
  -- - Buffer Objects ('VBO', 'ElementArray')
  -- - Textures ('Tex2D', 'CubeMap')
  -- - Shader 'Program's
  -- - Framebuffer Objects ('FBO')
  -- - Renderbuffer Objects ('RBO')

  -- * Binding Targets
  -- | If objects are referenced with integers (called names in GL) then
  -- binding targets can be thought of as global variables to put those
  -- references. Many operations implicitly read from these globals to 
  -- determine what the target object of the operation is. They include:
  --
  -- - Vertex array binding target (for VAO)
  -- - Buffer binding targets (ARRAY_BUFFER and ELEMENT_ARRAY_BUFFER)
  -- - Texture binding targets (TEXTURE_2D and TEXTURE_CUBE_MAP)
  -- - Framebuffer binding target (for FBO)
  --
  -- (not binding targets but similar)
  --
  -- - Shader program "in use"
  -- - Texture units
  -- - Current active texture unit
  -- - Image attachment points of an FBO

  -- * Shader Programs
  -- | The role of the second half of a program, the fragment shader, is to
  -- compute the color and depth of pixels covered by rasterized primitives
  -- (points, lines, and triangles) in the process of rendering. The role of
  -- the /first/ half of the program (vertex program) is to arrange the vertices
  -- of those primitives somewhere in clip space. Where these vertices and
  -- their attributes come from in the first place is determined by the VAO
  -- bound to the vertex array binding target. The program may also make use
  -- of uniform variables and texture units assigned by client code before
  -- rendering (but in a process separate from configuring the VAO). At most
  -- one Program can be "in use" at a time.

  -- * VAO
  -- | The VAO is essential. At least one VAO must be created and bound to the
  -- vertex array binding target before rendering, before configuring a
  -- program's vertex attributes. Here is why: the VAO stores the association
  -- between input variables in the program and a VBO from which to pipe input
  -- from. It also stores the format of the VBO data, which is otherwise just
  -- a big blob. Finally, the VAO stores the state of the element array binding
  -- target used for indexed rendering.
  --
  -- After installing a program with 'useProgram' and binding a source VBO
  -- to the array buffer binding target ('bindVBO') then the bound VAO can be
  -- updated ('setVertexAttributeLayout') with new vertex attribute information.
  -- After this, the VBO can be rebound to configure a different set of inputs
  -- with a different source. Many VAOs can be created and swapped out to pipe
  -- vertex data in different ways to different programs (or the same program).
  --
  -- When a VAO is bound it restores the state of the element array binding
  -- target. For this reason you can think of that binding target as simply
  -- being a function of the VAO itself rather than a separate global state.

  -- * Uniforms and Samplers (Textures)
  -- | Programs may have uniform variables and "sampler uniforms" as input.
  -- Uniforms are accessible from the vertex or fragment shader part of the
  -- program but their values are fixed during the course of a rendering command.
  -- They can be set and reset with the setUniform family (ex. 'setUniform1f'),
  -- which updates a program object with new uniform values. Among other
  -- things, updating the uniforms each frame is the main way to animate a
  -- scene.
  --
  -- Samplers are textures that the shader can interpolate to get "in between"
  -- values. The texture a sampler uses is determined by the contents of the
  -- texture unit that that sampler points to. The sampler is a uniform with
  -- an integer type. This integer is the texture unit to use. The word texture
  -- should not be construed to mean a color image. Shaders can make use of
  -- many kinds of multi-dimensional data that happen to be available through
  -- the samplers.

  -- * Texture Objects and Texture Units
  -- | Before a shader can use a texture it must be assigned to a texture unit.
  -- First set the active texture unit to the desired unit number
  -- ('setActiveTextureUnit') then bind the texture object to one of the
  -- two texture binding targets, depending on what kind of texture it is (2D
  -- or cubemap). Binding a texture has the side effect of assigning it to the
  -- active texture unit.

  -- * Custom Framebuffers
  -- | It is possible (and important to many techniques) to utilize an
  -- off-screen render target. To do this create an FBO ('newFBO'), bind it to
  -- the framebuffer binding target ('bindFramebuffer') and attach a color
  -- /image/ object (texture or renderbuffer object). If necessary a depth
  -- image or combination depth-stencil image can be attached as well. If no
  -- color image is attached then the FBO is incomplete and rendering will be
  -- an error.  After rendering to an FBO any textures that were attached can
  -- be used for a second pass by assigning them to a texture unit. Watch out
  -- for feedback loops accidentally sampling a texture that is also being
  -- rendered to at the same time!
  --
  -- A renderbuffer object is a minor character to be used when you do not
  -- expect to use the results of rendering but need an image anyway. For
  -- example you may need a depth buffer to do depth testing, or you may want
  -- to ignore the (required for rendering to work at all) color buffer.

  -- * Images and Image Formats
  -- | FBOs have attachment points for /images/. A texture serves as an image
  -- and a renderbuffer object serves as an image. Images have an "internal
  -- format" which describes the size and interpretation of pixel components.
  -- There are seven internal formats, five of which are color image formats
  -- such as grayscale and RGB. The other two are the depth buffer format and
  -- the combination depth-stencil format. RBOs ('newRBO') and empty textures
  -- ('newEmptyTexture2D', 'newEmptyCubeMap') can be created with any of these
  -- formats.
  --
  -- (The above is a gross simplification of OpenGL's image formats. I should
  -- probably revise, because it may greatly improve performance to use some
  -- of the 16-bit color formats rather than 32.)

  -- * Depth Testing, Stencil Testing, Scissor Testing, Facet Culling
  -- | The depth buffer and stencil buffers, if present in the current
  -- framebuffer, can be used to avoid rendering to points of the screen by
  -- testing against the value stored at those points. For example if commanded
  -- to show a triangle in a region of the framebuffer with a depth greater
  -- than current depth buffer values, then the triangle may not be rendered to
  -- the color buffer or anywhere else (depending on settings). There are many
  -- global settings to switch on and off these tests and the ability to
  -- modify the buffers involved. The stencil test in particular is highly
  -- configurable. The scissor test is the simplest: when activated nothing
  -- outside the scissor box (in screen space) will be rendered. The only
  -- other configuration is to set that scissor box ('setScissorBox').
  -- Polygons facing toward or away from the viewer can be dropped (or culled)
  -- from rendering with 'enableCulling'.

  -- * Coordinate Systems
  -- | - Screen space is simply the 2D coordinate system of your window.
  -- The viewport transformation (see 'setViewport') determines where in the
  -- window the mapping of the NDS cube (see below) will appear.
  -- - NDS, normalized device coordinates, or sometimes viewport space is a
  -- cube 2x2x2 centered at the origin the inside of which is your final scene,
  -- before it is mapped to the screen via the viewport setting (see
  -- 'setViewport'). If an orthographic projection was used to put the scene in
  -- clip space then clip space and NDS are the same.
  -- - Clip space is the destination of vertices transformed by the
  -- vertex program. Objects here are mapped to NDS using the perspective
  -- division technique to account for the case that the vertex shader used
  -- a perspective matrix.
  -- - Model space is the name for positions of raw vertices as present in
  -- the VBOs. The vertex program will want to somehow move these vertexes
  -- into clip space, representing generally the position and direction the
  -- user is viewing the scene from.

  -- * Rendering Points, Lines, and Triangles
  -- | The draw family (ex. 'drawTriangles') of commands commissions the
  -- rendering of a certain number of vertices worth of primitives. The
  -- current program will get input from the current VAO, the current texture
  -- units, and execute on all the potentially affected pixels in the current
  -- framebuffer. Vertexes are consumed in the order they appear in their
  -- respective source VBOs. If the VAO is missing, the program is missing, or
  -- the current framebuffer has no color attachment, then rendering will not
  -- work.
  --
  -- The drawIndexed family (ex. 'drawIndexedTriangles') of commands carries
  -- out the same effects as the non-indexed rendering commands but traverses
  -- vertices in an order determined by the sequence of indexes packed in the
  -- ElementArray currently bound to the element array binding target. This
  -- mainly allows a huge reuse of vertex data in the case that the object
  -- being rendered forms a closed mesh.

  -- * VAO
  VAO,
  newVAO,
  bindVAO,
  deleteVAO,

  -- * Buffer Objects
  -- ** VBO
  VBO,
  UsageHint(..),
  newVBO,
  bindVBO,
  updateVBO,
  deleteVBO,

  -- ** Element Array
  ElementArray,
  IndexFormat(..),
  newElementArray,
  bindElementArray,
  updateElementArray,
  deleteElementArray,

  -- * Shader Program
  Program,
  ProgramError(..),
  newProgram,
  newProgramSafe,
  useProgram,
  deleteProgram,

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
  Dimensions(..),
  Cube(..),
  Side,
  newTexture2D,
  newCubeMap,
  newEmptyTexture2D,
  newEmptyCubeMap,
  deleteTexture,
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

  -- ** Blending
  BlendFactor(..),
  BlendEquation(..),
  enableBlending,
  disableBlending,
  setBlendFactors,
  setBlendEquation,

  -- ** Viewport
  Viewport(..),
  setViewport,

  -- * Framebuffers
  DefaultFramebuffer,
  FBO,
  bindFramebuffer,
  newFBO,
  attachTex2D,
  attachCubeMap,
  attachRBO,
  deleteFBO,

  -- * Renderbuffers
  RBO,
  newRBO,
  deleteRBO,

  -- * Errors
  GLError(..),
  getGLError,

  -- * Image Formats
  Alpha,
  Luminance,
  LuminanceAlpha,
  RGB,
  RGBA,
  Depth24,
  Depth24Stencil8,

  -- * Classes
  InternalFormat(..),
  Framebuffer(..),
  Texture(..),
  Attachable(..)

) where

import Prelude hiding (sum)
import Control.Exception
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.String
import Data.Vector.Storable (Vector, unsafeWith)
import qualified Data.Vector.Storable as V (length)
import Control.Monad hiding (forM_)
import Data.Word
import Data.Int
import Data.Functor
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Default

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

-- | A VBO is a buffer object which has vertex data. Shader programs use VBOs
-- as input to their vertex attributes according to the configuration of the
-- bound VAO.
data VBO = VBO GLuint deriving Show

-- | A buffer object which has a packed sequence of vertex indices. Indexed
-- rendering uses the ElementArray bound to the element array binding target.
data ElementArray = ElementArray GLuint deriving Show

-- | A 2D texture. A program can sample a texture if it has been bound to
-- the appropriate texture unit.
newtype Tex2D a = Tex2D GLuint deriving Show

-- | A cubemap texture is just six 2D textures. A program can sample a cubemap
-- texture if it has been bound to the appropriate texture unit.
newtype CubeMap a = CubeMap GLuint deriving Show

-- | A framebuffer object is an alternative rendering target. Once an FBO is
-- bound to framebuffer binding target, it is possible to attach images
-- (textures or RBOs) for color, depth, or stencil rendering.
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
  

-- | Usage hint for allocation of buffer object storage.
data UsageHint = StaticDraw  -- ^ Data will seldomly change.
               | DynamicDraw -- ^ Data will change.
               | StreamDraw  -- ^ Data will change very often.
                 deriving Show

instance ToGL UsageHint where
  toGL StreamDraw  = GL_STREAM_DRAW
  toGL StaticDraw  = GL_STATIC_DRAW
  toGL DynamicDraw = GL_DYNAMIC_DRAW

-- | RGBA color quad.
data Color = Color !Float !Float !Float !Float deriving Show


-- | 1-byte alpha channel only.
data Alpha = Alpha deriving Show

-- | 1-byte grayscale pixel format.
data Luminance = Luminance deriving Show

-- | 2-byte luminance and alpha channel format.
data LuminanceAlpha = Luminancealpha deriving Show

-- | 3-byte true color pixel format.
data RGB = RGB deriving Show

-- | 4-byte true color plus alpha channel format.
data RGBA = RGBA deriving Show

-- | 24-bit depth format.
data Depth24 = Depth24 deriving Show

-- | Combination depth and stencil format.
data Depth24Stencil8 = Depth24Stencil8 deriving Show

-- | OpenGL internal image formats.
class InternalFormat a where
  internalFormat :: (Eq b, Num b) => proxy a -> b
instance InternalFormat RGB where
  internalFormat _ = GL_RGB8
instance InternalFormat RGBA where
  internalFormat _ = GL_RGBA
instance InternalFormat Alpha where
  internalFormat _ = GL_ALPHA
instance InternalFormat Luminance where
  internalFormat _ = GL_LUMINANCE
instance InternalFormat LuminanceAlpha where
  internalFormat _ = GL_LUMINANCE_ALPHA
instance InternalFormat Depth24 where
  internalFormat _ = GL_DEPTH_COMPONENT24
instance InternalFormat Depth24Stencil8 where
  internalFormat _ = GL_DEPTH24_STENCIL8

-- | The allowed attachment point for images with an internal format.
class InternalFormat a => Attachable a where
  attachPoint :: (Eq b, Num b) => proxy a -> b
instance Attachable RGB where
  attachPoint _ = GL_COLOR_ATTACHMENT0
instance Attachable RGBA where
  attachPoint _ = GL_COLOR_ATTACHMENT0
instance Attachable Luminance where
  attachPoint _ = GL_COLOR_ATTACHMENT0
instance Attachable LuminanceAlpha where
  attachPoint _ = GL_COLOR_ATTACHMENT0
instance Attachable Alpha where
  attachPoint _ = GL_COLOR_ATTACHMENT0
instance Attachable Depth24 where
  attachPoint _ = GL_DEPTH_ATTACHMENT
instance Attachable Depth24Stencil8 where
  attachPoint _ = GL_DEPTH_STENCIL_ATTACHMENT

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


-- | An RBO is a kind of image object used for rendering. The only thing
-- you can do with an RBO is attach it to an FBO.
data RBO a = RBO { unRBO :: GLuint } deriving Show

-- | A rectangular section of the window.
data Viewport = Viewport
  { viewportX :: Int
  , viewportY :: Int
  , viewportW :: Int
  , viewportH :: Int }
    deriving (Eq, Show)

-- | The size of an image in pixels, parameterized by an image format type.
data Dimensions = Dimensions
  { imageWidth :: Int
  , imageHeight :: Int }
    deriving (Show)

-- | Six values, one on each side.
data Cube a = Cube
  { cubeRight  :: a
  , cubeLeft   :: a
  , cubeTop    :: a
  , cubeBottom :: a
  , cubeFront  :: a
  , cubeBack   :: a }
    deriving (Show, Functor, Foldable, Traversable)

-- | A type to pick one of the sides of a cube. See the accessors of the
-- type 'Cube'.
type Side = forall a . Cube a -> a

instance Applicative Cube where
  pure x = Cube x x x x x x
  (Cube f1 f2 f3 f4 f5 f6) <*> (Cube x1 x2 x3 x4 x5 x6) =
    Cube (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6)

-- | Either a vertex shader or a fragment shader.
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

-- | Detectable errors.
data GLError =
  InvalidEnum | -- ^ Enum argument out of range.
  InvalidValue | -- ^ Integer argument out of range.
  InvalidOperation | -- ^ Operation illegal in current state.
  InvalidFramebufferOperation | -- ^ Framebuffer is not complete.
  OutOfMemory
    deriving Typeable

instance Exception GLError

instance Show GLError where
  show InvalidEnum = "INVALID_ENUM enum argument out of range"
  show InvalidValue = "INVALID_VALUE Numeric argument out of range"
  show InvalidOperation = "INVALID_OPERATION Illegal in current state"
  show InvalidFramebufferOperation = "INVALID_FRAMEBUFFER_OPERATION Framebuffer object is not complete"
  show OutOfMemory = "Not enough memory left to execute command"

class ToGL a where
  toGL :: (Num b, Eq b) => a -> b

-- | Textures have an internal numeric name.
class Texture a where
  textureName :: Num b => a -> b

instance Texture (Tex2D a) where
  textureName (Tex2D n) = fromIntegral n

instance Texture (CubeMap a) where
  textureName (CubeMap n) = fromIntegral n


-- | Blending functions for alpha blending.
data BlendEquation =
  FuncAdd | -- ^ the default
  FuncSubtract |
  FuncReverseSubtract
    deriving Show

instance Default BlendEquation where
  def = FuncAdd

-- | Blending factors.
data BlendFactor =
  BlendOne |
  BlendZero |
  BlendSourceAlpha |
  BlendOneMinusSourceAlpha
    deriving Show


-- | The default framebuffer. Bind this to render to the screen as usual.
-- Use the Default instance method 'def' to construct it.
data DefaultFramebuffer = DefaultFramebuffer deriving Show

instance Default DefaultFramebuffer where
  def = DefaultFramebuffer

-- | Framebuffers can be bound to the framebuffer binding target. There is
-- a default framebuffer and the client may create an arbitrary number of
-- new framebuffer objects.
class Framebuffer a where
  framebufferName :: Num b => a -> b

instance Framebuffer DefaultFramebuffer where
  framebufferName _ = 0

instance Framebuffer FBO where
  framebufferName (FBO n) = fromIntegral n



-- | Create a new VAO. The only thing you can do with a VAO is bind it to
-- the vertex array binding target.
newVAO :: IO VAO
newVAO = do
  n <- alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)
  return (VAO n)

-- | Delete a VAO.
deleteVAO :: VAO -> IO ()
deleteVAO (VAO n) = withArray [n] (\ptr -> glDeleteVertexArrays 1 ptr)

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
  return (VBO n)

-- | Delete a VBO.
deleteVBO :: VBO -> IO ()
deleteVBO (VBO n) = withArray [n] (\ptr -> glDeleteBuffers 1 ptr)

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

-- | Bind a VBO to the array buffer binding target. The buffer object bound
-- there will be replaced, if any.
bindVBO :: VBO -> IO ()
bindVBO (VBO n) = glBindBuffer GL_ARRAY_BUFFER n


-- | Create a new ElementArray buffer object from the blob of packed indices.
-- The usage argument hints at how often you plan to modify the data.
newElementArray :: Vector Word8 -> UsageHint -> IO ElementArray
newElementArray bytes usage = do
  n <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER n
  let len = V.length bytes
  unsafeWith bytes $ \ptr -> do
    glBufferData
      GL_ELEMENT_ARRAY_BUFFER
      (fromIntegral len)
      (castPtr ptr)
      (toGL usage)
  return (ElementArray n)

-- | Delete an ElementArray
deleteElementArray :: ElementArray -> IO ()
deleteElementArray (ElementArray n) = withArray [n] (\ptr -> glDeleteBuffers 1 ptr)
  
-- | Modify contents in the currently bound ElementArray starting at the
-- specified index in bytes.
updateElementArray :: Vector Word8 -> Int -> IO ()
updateElementArray bytes offset = unsafeWith bytes $ \ptr -> do
  glBufferSubData
    GL_ELEMENT_ARRAY_BUFFER
    (fromIntegral offset)
    (fromIntegral (V.length bytes))
    (castPtr ptr)


-- | Assign an ElementArray to the element array binding target. It will
-- replace the ElementArray already bound there, if any. Note that the state
-- of the element array binding target is a function of the current VAO.
bindElementArray :: ElementArray -> IO ()
bindElementArray (ElementArray n) = glBindBuffer GL_ELEMENT_ARRAY_BUFFER n


-- | Same as 'newProgram' but does not throw exceptions.
newProgramSafe :: String -> String -> IO (Either ProgramError Program)
newProgramSafe vcode fcode = try $ newProgram vcode fcode

-- | Delete a program.
deleteProgram :: Program -> IO ()
deleteProgram (Program n) = glDeleteProgram n

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
deleteTexture x = withArray [textureName x] (\ptr -> glDeleteTextures 1 ptr)

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

  

-- | Allow rendering commands to modify the color buffer of the current
-- framebuffer.
enableColorWriting :: IO ()
enableColorWriting = glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE

-- | Disable rendering to color buffer.
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

-- | Enable the stencil test. Any pixels rendered to the screen where the
-- stencil buffer is 1 will not be rendered. This disables writing to the
-- stencil buffer.
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

-- | Allow rendering to modify the stencil buffer. Any pixels rendered to
-- the screen will set the stencil buffer to 1 at that location.
enableStencilWriting :: IO ()
enableStencilWriting = do
  glStencilFunc GL_ALWAYS 1 maxBound
  glStencilOp GL_KEEP GL_KEEP GL_REPLACE
  glStencilMask 1

-- | Disable rendering to the stencil buffer.
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

-- | Set the viewport. The default viewport simply covers the entire window.
setViewport :: Viewport -> IO ()
setViewport (Viewport x y w h) =
  glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

-- | Binds an FBO or the default framebuffer to the framebuffer binding target.
-- Replaces the framebuffer already bound there.
bindFramebuffer :: Framebuffer a => a -> IO ()
bindFramebuffer x = glBindFramebuffer GL_FRAMEBUFFER (framebufferName x)

-- | Create a new framebuffer object. Before the framebuffer can be used for
-- rendering it must have a color image attachment.
newFBO :: IO FBO
newFBO = do
  n <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
  return (FBO n)

-- | Delete an FBO.
deleteFBO :: FBO -> IO ()
deleteFBO (FBO n) = withArray [n] (\ptr -> glDeleteFramebuffers 1 ptr)

-- | Attach a 2D texture to the FBO currently bound to the
-- framebuffer binding target.
attachTex2D :: Attachable a => Tex2D a -> IO ()
attachTex2D t@(Tex2D n) =
  glFramebufferTexture2D GL_FRAMEBUFFER (attachPoint t) GL_TEXTURE_2D n 0

-- | Attach one of the sides of a cubemap texture to the FBO currently bound
-- to the framebuffer binding target.
attachCubeMap :: Attachable a => CubeMap a -> Side -> IO ()
attachCubeMap cm@(CubeMap n) side =
  glFramebufferTexture2D
    GL_FRAMEBUFFER
    (attachPoint cm)
    (side cubeSideCodes)
    n
    0

cubeSideCodes :: Cube GLenum
cubeSideCodes = Cube
  { cubeLeft   = GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  , cubeRight  = GL_TEXTURE_CUBE_MAP_POSITIVE_X
  , cubeTop    = GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  , cubeBottom = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  , cubeFront  = GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  , cubeBack   = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z }

-- | Attach an RBO to the FBO currently bound to the framebuffer binding
-- target.
attachRBO :: Attachable a => RBO a -> IO ()
attachRBO rbo = glFramebufferRenderbuffer
  GL_FRAMEBUFFER (attachPoint rbo) GL_RENDERBUFFER (unRBO rbo)

-- | Create a new renderbuffer with the specified dimensions.
newRBO :: InternalFormat a => Int -> Int -> IO (RBO a)
newRBO w h = do
  n <- alloca (\ptr -> glGenRenderbuffers 1 ptr >> peek ptr)
  rbo <- return (RBO n)
  glBindRenderbuffer GL_RENDERBUFFER n
  glRenderbufferStorage
    GL_RENDERBUFFER
    (internalFormat rbo)
    (fromIntegral w)
    (fromIntegral h)
  return rbo

-- | Delete an RBO.
deleteRBO :: RBO a -> IO ()
deleteRBO (RBO n) = withArray [n] (\ptr -> glDeleteRenderbuffers 1 ptr)


-- | Enable alpha blending.
enableBlending :: IO ()
enableBlending = return ()

-- | Disable alpha blending.
disableBlending :: IO ()
disableBlending = return ()

-- | Set the computation for source and destination blending factors.
setBlendFactors :: BlendFactor -> BlendFactor -> IO ()
setBlendFactors s d = return ()

-- | Set the overall blending function.
setBlendEquation :: BlendEquation -> IO ()
setBlendEquation e = return ()



-- | Check for a GL Error.
getGLError :: IO (Maybe GLError)
getGLError = do
  n <- glGetError
  return $ case n of
    GL_NO_ERROR -> Nothing
    GL_INVALID_ENUM -> Just InvalidEnum
    GL_INVALID_VALUE -> Just InvalidValue
    GL_INVALID_OPERATION -> Just InvalidOperation
    GL_INVALID_FRAMEBUFFER_OPERATION -> Just InvalidFramebufferOperation
    GL_OUT_OF_MEMORY -> Just OutOfMemory
    _ -> error ("unknown GL error " ++ show n)
