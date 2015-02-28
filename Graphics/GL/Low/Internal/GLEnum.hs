{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Low.Internal.GLEnum where

import Data.Data
import Graphics.GL
import Graphics.GL.Low.Classes (ToGL(..), FromGL(..))


-- | Either a vertex shader or a fragment shader.
data ShaderType = VertexShader | FragmentShader 
  deriving (Eq, Ord, Show, Read)

instance ToGL ShaderType where
  toGL VertexShader = GL_VERTEX_SHADER
  toGL FragmentShader = GL_FRAGMENT_SHADER



-- | Blending functions.
data BlendEquation =
  FuncAdd | -- ^ the default
  FuncSubtract |
  FuncReverseSubtract
    deriving (Eq, Ord, Show, Read)

instance ToGL BlendEquation where
  toGL FuncAdd = GL_FUNC_ADD
  toGL FuncSubtract = GL_FUNC_SUBTRACT
  toGL FuncReverseSubtract = GL_FUNC_REVERSE_SUBTRACT


-- | Blending factors.
data BlendFactor =
  BlendOne |
  BlendZero |
  BlendSourceColor |
  BlendOneMinusSourceColor |
  BlendDestColor |
  BlendOneMinusDestColor |
  BlendSourceAlpha |
  BlendOneMinusSourceAlpha |
  BlendDestAlpha |
  BlendOneMinusDestAlpha |
  BlendConstantColor |
  BlendOneMinusConstantColor |
  BlendConstantAlpha |
  BlendOneMinusConstantAlpha
    deriving Show

instance ToGL BlendFactor where
  toGL BlendOne = GL_ONE
  toGL BlendZero = GL_ZERO
  toGL BlendSourceColor = GL_SRC_COLOR
  toGL BlendOneMinusSourceColor = GL_ONE_MINUS_SRC_COLOR
  toGL BlendDestColor = GL_DST_COLOR
  toGL BlendOneMinusDestColor = GL_ONE_MINUS_DST_COLOR
  toGL BlendSourceAlpha = GL_SRC_ALPHA
  toGL BlendOneMinusSourceAlpha = GL_ONE_MINUS_SRC_ALPHA
  toGL BlendDestAlpha = GL_DST_ALPHA
  toGL BlendOneMinusDestAlpha = GL_ONE_MINUS_DST_ALPHA
  toGL BlendConstantColor = GL_ONE_MINUS_CONSTANT_COLOR
  toGL BlendOneMinusConstantColor = GL_ONE_MINUS_CONSTANT_COLOR
  toGL BlendConstantAlpha = GL_CONSTANT_ALPHA
  toGL BlendOneMinusConstantAlpha = GL_ONE_MINUS_CONSTANT_ALPHA



-- | Usage hint for allocation of buffer object storage.
data UsageHint = StaticDraw  -- ^ Data will seldomly change.
               | DynamicDraw -- ^ Data will change.
               | StreamDraw  -- ^ Data will change very often.
                 deriving Show

instance ToGL UsageHint where
  toGL StaticDraw  = GL_STATIC_DRAW
  toGL DynamicDraw = GL_DYNAMIC_DRAW
  toGL StreamDraw  = GL_STREAM_DRAW



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




-- | The stencil test passes under what condition.
data StencilFunc =
  Never |
  Less |
  LessOrEqual |
  Greater |
  GreaterOrEqual |
  Equal |
  NotEqual |
  Always
    deriving Show

instance ToGL StencilFunc where
  toGL x = case x of
    Never -> GL_NEVER
    Less -> GL_LESS
    LessOrEqual -> GL_LEQUAL
    Greater -> GL_GREATER
    GreaterOrEqual -> GL_GEQUAL
    Equal -> GL_EQUAL
    NotEqual -> GL_NOTEQUAL
    Always -> GL_ALWAYS


-- | Modification action for the stencil buffer. 
data StencilOp =
  Keep | -- ^ Do nothing.
  Zero | -- ^ Set to zero.
  Replace | -- ^ Write the ref value passed to enableStencil.
  Increment |
  Decrement |
  Invert | -- ^ Bitwise complement.
  IncrementWrap |
  DecrementWrap
    deriving Show

instance ToGL StencilOp where
  toGL x = case x of
    Keep -> GL_KEEP
    Zero -> GL_ZERO
    Replace -> GL_REPLACE
    Increment -> GL_INCR
    Decrement -> GL_DECR
    Invert -> GL_INVERT
    IncrementWrap -> GL_INCR_WRAP
    DecrementWrap -> GL_DECR_WRAP

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


data AttribType =
  GLFloat         | -- ^ 4-byte float
  GLDouble        | -- ^ 8-byte float
  GLByte          | -- ^ signed byte
  GLUnsignedByte  | -- ^ unsigned byte
  GLShort         | -- ^ 2-byte signed integer
  GLUnsignedShort | -- ^ 2-byte unsigned integer
  GLInt           | -- ^ 4-byte signed integer
  GLUnsignedInt     -- ^ 4-byte unsigned integer
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)

instance ToGL AttribType where
  toGL x = case x of
    GLFloat         -> GL_FLOAT
    GLDouble        -> GL_DOUBLE
    GLByte          -> GL_BYTE
    GLUnsignedByte  -> GL_UNSIGNED_BYTE
    GLShort         -> GL_SHORT
    GLUnsignedShort -> GL_UNSIGNED_SHORT
    GLInt           -> GL_INT
    GLUnsignedInt   -> GL_UNSIGNED_INT

