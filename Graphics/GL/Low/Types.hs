module Graphics.GL.Low.Types where

import Graphics.GL

import Graphics.GL.Low.Classes

-- | Handle to a VAO.
newtype VAO = VAO { fromVAO :: GLuint }
  deriving Show

-- | Handle to a buffer object, such as a VBO or an element array.
newtype BufferObject = BufferObject { fromBufferObject :: GLuint }
  deriving Show

-- | A framebuffer object is an alternative rendering destination. Once an FBO
-- is bound to framebuffer binding target, it is possible to attach images
-- (textures or RBOs) for color, depth, or stencil rendering.
newtype FBO = FBO { fromFBO :: GLuint }
  deriving Show

-- | An RBO is a kind of image object used for rendering. The only thing
-- you can do with an RBO is attach it to an FBO.
data RBO = RBO
  { rboObjectName :: GLuint
  , rboFormat     :: ImageFormat } 
    deriving Show

-- | Handle to a texture object. It may be a Tex2D or a cubemap.
data Texture = Texture
  { texObjectName  :: GLuint
  , texFormat      :: ImageFormat }

-- | Handle to a shader program.
newtype Program = Program { fromProgram :: GLuint } 
  deriving Show

-- | Handle to a shader object.
newtype Shader = Shader { fromShader :: GLuint } 
  deriving Show

data ImageFormat =
  RGB |
  RGBA |
  Alpha |
  Luminance |
  LuminanceAlpha |
  Depth24 |
  Depth24Stencil8
    deriving (Show, Read, Eq, Ord)

instance ToGL ImageFormat where
  toGL imf = case imf of
    RGB -> GL_RGB
    RGBA -> GL_RGBA
    Alpha -> GL_ALPHA
    Luminance -> GL_LUMINANCE
    LuminanceAlpha -> GL_LUMINANCE_ALPHA
    Depth24 -> GL_DEPTH_COMPONENT24
    Depth24Stencil8 -> GL_DEPTH24_STENCIL8


