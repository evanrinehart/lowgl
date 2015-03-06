{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Low.Internal.GLEnum where

import Data.Data
import Graphics.GL
import Graphics.GL.Low.Classes (ToGL(..), FromGL(..))


-- | Either a vertex shader or a fragment shader.
data ShaderType = VertexShader | FragmentShader 
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance ToGL ShaderType where
  toGL VertexShader = GL_VERTEX_SHADER
  toGL FragmentShader = GL_FRAGMENT_SHADER



-- | Blending functions.
data BlendEquation =
  FuncAdd | -- ^ the default
  FuncSubtract |
  FuncReverseSubtract
    deriving (Eq, Ord, Show, Read, Data, Typeable)

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
    deriving (Eq, Ord, Show, Read, Data, Typeable)

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
                 deriving (Eq, Ord, Show, Read, Data, Typeable)

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




data GLFloatType = Single | Double
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)

data Signedness = Signed | Unsigned
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)

data GLScalarType = GLFloat GLFloatType
                  | GLInteger Signedness
  deriving (Eq, Ord, Read, Show, Data, Typeable)

instance ToGL GLScalarType where
  toGL (GLFloat Single)     = GL_FLOAT
  toGL (GLFloat Double)     = GL_DOUBLE
  toGL (GLInteger Signed)   = GL_INT
  toGL (GLInteger Unsigned) = GL_UNSIGNED_INT

instance FromGL GLScalarType where
  fromGL GL_FLOAT        = Just $ GLFloat Single
  fromGL GL_DOUBLE       = Just $ GLFloat Double
  fromGL GL_INT          = Just $ GLInteger Signed
  fromGL GL_UNSIGNED_INT = Just $ GLInteger Unsigned
  fromGL _               = Nothing


data GLVectorSize = Two | Three | Four
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)

data GLAttribType = GLScalarAttrib GLScalarType
                  | GLVectorAttrib GLScalarType GLVectorSize 
                  | GLMatrixAttrib GLFloatType  GLVectorSize GLVectorSize 
  deriving (Eq, Ord, Read, Show, Data, Typeable)

instance ToGL GLAttribType where
    toGL (GLScalarAttrib s) = toGL s
    toGL (GLVectorAttrib (GLFloat Single) Two  ) = GL_FLOAT_VEC2
    toGL (GLVectorAttrib (GLFloat Single) Three) = GL_FLOAT_VEC3
    toGL (GLVectorAttrib (GLFloat Single) Four ) = GL_FLOAT_VEC4
    toGL (GLVectorAttrib (GLFloat Double) Two  ) = GL_DOUBLE_VEC2
    toGL (GLVectorAttrib (GLFloat Double) Three) = GL_DOUBLE_VEC3
    toGL (GLVectorAttrib (GLFloat Double) Four ) = GL_DOUBLE_VEC4
    toGL (GLVectorAttrib (GLInteger Signed)   Two  ) = GL_INT_VEC2
    toGL (GLVectorAttrib (GLInteger Signed)   Three) = GL_INT_VEC3
    toGL (GLVectorAttrib (GLInteger Signed)   Four ) = GL_INT_VEC4
    toGL (GLVectorAttrib (GLInteger Unsigned) Two  ) = GL_UNSIGNED_INT_VEC2
    toGL (GLVectorAttrib (GLInteger Unsigned) Three) = GL_UNSIGNED_INT_VEC3
    toGL (GLVectorAttrib (GLInteger Unsigned) Four ) = GL_UNSIGNED_INT_VEC4
    toGL (GLMatrixAttrib Single Two   Two  ) = GL_FLOAT_MAT2
    toGL (GLMatrixAttrib Single Two   Three) = GL_FLOAT_MAT2x3
    toGL (GLMatrixAttrib Single Two   Four ) = GL_FLOAT_MAT2x4
    toGL (GLMatrixAttrib Single Three Two  ) = GL_FLOAT_MAT3x2
    toGL (GLMatrixAttrib Single Three Three) = GL_FLOAT_MAT3
    toGL (GLMatrixAttrib Single Three Four ) = GL_FLOAT_MAT3x4
    toGL (GLMatrixAttrib Single Four  Two  ) = GL_FLOAT_MAT4x2
    toGL (GLMatrixAttrib Single Four  Three) = GL_FLOAT_MAT4x3
    toGL (GLMatrixAttrib Single Four  Four ) = GL_FLOAT_MAT4
    toGL (GLMatrixAttrib Double Two   Two  ) = GL_DOUBLE_MAT2
    toGL (GLMatrixAttrib Double Two   Three) = GL_DOUBLE_MAT2x3
    toGL (GLMatrixAttrib Double Two   Four ) = GL_DOUBLE_MAT2x4
    toGL (GLMatrixAttrib Double Three Two  ) = GL_DOUBLE_MAT3x2
    toGL (GLMatrixAttrib Double Three Three) = GL_DOUBLE_MAT3
    toGL (GLMatrixAttrib Double Three Four ) = GL_DOUBLE_MAT3x4
    toGL (GLMatrixAttrib Double Four  Two  ) = GL_DOUBLE_MAT4x2
    toGL (GLMatrixAttrib Double Four  Three) = GL_DOUBLE_MAT4x3
    toGL (GLMatrixAttrib Double Four  Four ) = GL_DOUBLE_MAT4

instance FromGL GLAttribType where
    fromGL GL_FLOAT             = Just $ GLScalarAttrib (GLFloat Single)
    fromGL GL_DOUBLE            = Just $ GLScalarAttrib (GLFloat Double)
    fromGL GL_INT               = Just $ GLScalarAttrib (GLInteger Signed)
    fromGL GL_UNSIGNED_INT      = Just $ GLScalarAttrib (GLInteger Unsigned)
    fromGL GL_FLOAT_VEC2        = Just $ GLVectorAttrib (GLFloat Single) Two  
    fromGL GL_FLOAT_VEC3        = Just $ GLVectorAttrib (GLFloat Single) Three
    fromGL GL_FLOAT_VEC4        = Just $ GLVectorAttrib (GLFloat Single) Four 
    fromGL GL_DOUBLE_VEC2       = Just $ GLVectorAttrib (GLFloat Double) Two  
    fromGL GL_DOUBLE_VEC3       = Just $ GLVectorAttrib (GLFloat Double) Three
    fromGL GL_DOUBLE_VEC4       = Just $ GLVectorAttrib (GLFloat Double) Four 
    fromGL GL_INT_VEC2          = Just $ GLVectorAttrib (GLInteger Signed)   Two  
    fromGL GL_INT_VEC3          = Just $ GLVectorAttrib (GLInteger Signed)   Three
    fromGL GL_INT_VEC4          = Just $ GLVectorAttrib (GLInteger Signed)   Four 
    fromGL GL_UNSIGNED_INT_VEC2 = Just $ GLVectorAttrib (GLInteger Unsigned) Two  
    fromGL GL_UNSIGNED_INT_VEC3 = Just $ GLVectorAttrib (GLInteger Unsigned) Three
    fromGL GL_UNSIGNED_INT_VEC4 = Just $ GLVectorAttrib (GLInteger Unsigned) Four 
    fromGL GL_FLOAT_MAT2        = Just $ GLMatrixAttrib Single Two   Two  
    fromGL GL_FLOAT_MAT2x3      = Just $ GLMatrixAttrib Single Two   Three
    fromGL GL_FLOAT_MAT2x4      = Just $ GLMatrixAttrib Single Two   Four 
    fromGL GL_FLOAT_MAT3x2      = Just $ GLMatrixAttrib Single Three Two  
    fromGL GL_FLOAT_MAT3        = Just $ GLMatrixAttrib Single Three Three
    fromGL GL_FLOAT_MAT3x4      = Just $ GLMatrixAttrib Single Three Four 
    fromGL GL_FLOAT_MAT4x2      = Just $ GLMatrixAttrib Single Four  Two  
    fromGL GL_FLOAT_MAT4x3      = Just $ GLMatrixAttrib Single Four  Three
    fromGL GL_FLOAT_MAT4        = Just $ GLMatrixAttrib Single Four  Four 
    fromGL GL_DOUBLE_MAT2       = Just $ GLMatrixAttrib Double Two   Two  
    fromGL GL_DOUBLE_MAT2x3     = Just $ GLMatrixAttrib Double Two   Three
    fromGL GL_DOUBLE_MAT2x4     = Just $ GLMatrixAttrib Double Two   Four 
    fromGL GL_DOUBLE_MAT3x2     = Just $ GLMatrixAttrib Double Three Two  
    fromGL GL_DOUBLE_MAT3       = Just $ GLMatrixAttrib Double Three Three
    fromGL GL_DOUBLE_MAT3x4     = Just $ GLMatrixAttrib Double Three Four 
    fromGL GL_DOUBLE_MAT4x2     = Just $ GLMatrixAttrib Double Four  Two  
    fromGL GL_DOUBLE_MAT4x3     = Just $ GLMatrixAttrib Double Four  Three
    fromGL GL_DOUBLE_MAT4       = Just $ GLMatrixAttrib Double Four  Four 
    fromGL _                    = Nothing


data GLUniformScalar = FloatSampler | IntSampler Signedness
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data GLSamplerType = Sampler1D      | Sampler1DArray
                   | Sampler2D      | Sampler2DArray 
                   | Multisampler2D | Multisampler2DArray
                   | Sampler3D      | SamplerCube
                   | SamplerBuffer  | Sampler2DRect
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)

data GLShadowSamplerType = ShadowSampler1D     | ShadowSampler1DArray
                         | ShadowSampler2D     | ShadowSampler2DArray
                         | ShadowSampler2DRect | ShadowSamplerCube
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)

data GLUniformType = GLScalarUniform GLScalarType
                   | GLVectorUniform GLScalarType GLVectorSize 
                   | GLMatrixUniform GLFloatType  GLVectorSize GLVectorSize 
                   | GLBoolUniform (Maybe GLVectorSize)
                   | GLSamplerUniform GLUniformScalar GLSamplerType 
                   | GLImageUniform   GLUniformScalar GLSamplerType 
                   | GLShadowSampler GLShadowSamplerType
                   | GLAtomicCounter
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance ToGL GLUniformType where
    toGL (GLScalarUniform s) = toGL $ GLScalarAttrib s
    toGL (GLVectorUniform s n) = toGL $ GLVectorAttrib s n
    toGL (GLMatrixUniform s n m) = toGL $ GLMatrixAttrib s n m
    toGL (GLBoolUniform Nothing     ) = GL_BOOL      
    toGL (GLBoolUniform (Just Two)  ) = GL_BOOL_VEC2 
    toGL (GLBoolUniform (Just Three)) = GL_BOOL_VEC3 
    toGL (GLBoolUniform (Just Four) ) = GL_BOOL_VEC4 
    toGL (GLSamplerUniform FloatSampler Sampler1D          ) = GL_SAMPLER_1D                       
    toGL (GLSamplerUniform FloatSampler Sampler2D          ) = GL_SAMPLER_2D                   
    toGL (GLSamplerUniform FloatSampler Sampler3D          ) = GL_SAMPLER_3D                   
    toGL (GLSamplerUniform FloatSampler Sampler2DRect      ) = GL_SAMPLER_2D_RECT              
    toGL (GLSamplerUniform FloatSampler SamplerCube        ) = GL_SAMPLER_CUBE                 
    toGL (GLSamplerUniform FloatSampler SamplerBuffer      ) = GL_SAMPLER_BUFFER               
    toGL (GLSamplerUniform FloatSampler Sampler1DArray     ) = GL_SAMPLER_1D_ARRAY             
    toGL (GLSamplerUniform FloatSampler Sampler2DArray     ) = GL_SAMPLER_2D_ARRAY             
    toGL (GLSamplerUniform FloatSampler Multisampler2D     ) = GL_SAMPLER_2D_MULTISAMPLE       
    toGL (GLSamplerUniform FloatSampler Multisampler2DArray) = GL_SAMPLER_2D_MULTISAMPLE_ARRAY 
    toGL (GLShadowSampler ShadowSampler1D     ) = GL_SAMPLER_1D_SHADOW      
    toGL (GLShadowSampler ShadowSampler2D     ) = GL_SAMPLER_2D_SHADOW      
    toGL (GLShadowSampler ShadowSampler2DRect ) = GL_SAMPLER_2D_RECT_SHADOW 
    toGL (GLShadowSampler ShadowSamplerCube   ) = GL_SAMPLER_CUBE_SHADOW    
    toGL (GLShadowSampler ShadowSampler1DArray) = GL_SAMPLER_1D_ARRAY_SHADOW
    toGL (GLShadowSampler ShadowSampler2DArray) = GL_SAMPLER_2D_ARRAY_SHADOW
    toGL (GLSamplerUniform (IntSampler Signed) Sampler1D          ) = GL_INT_SAMPLER_1D                  
    toGL (GLSamplerUniform (IntSampler Signed) Sampler2D          ) = GL_INT_SAMPLER_2D                  
    toGL (GLSamplerUniform (IntSampler Signed) Sampler3D          ) = GL_INT_SAMPLER_3D                  
    toGL (GLSamplerUniform (IntSampler Signed) Sampler2DRect      ) = GL_INT_SAMPLER_2D_RECT             
    toGL (GLSamplerUniform (IntSampler Signed) SamplerCube        ) = GL_INT_SAMPLER_CUBE                
    toGL (GLSamplerUniform (IntSampler Signed) SamplerBuffer      ) = GL_INT_SAMPLER_BUFFER              
    toGL (GLSamplerUniform (IntSampler Signed) Sampler1DArray     ) = GL_INT_SAMPLER_1D_ARRAY            
    toGL (GLSamplerUniform (IntSampler Signed) Sampler2DArray     ) = GL_INT_SAMPLER_2D_ARRAY            
    toGL (GLSamplerUniform (IntSampler Signed) Multisampler2D     ) = GL_INT_SAMPLER_2D_MULTISAMPLE      
    toGL (GLSamplerUniform (IntSampler Signed) Multisampler2DArray) = GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
    toGL (GLSamplerUniform (IntSampler Unsigned) Sampler1D          ) = GL_UNSIGNED_INT_SAMPLER_1D                  
    toGL (GLSamplerUniform (IntSampler Unsigned) Sampler2D          ) = GL_UNSIGNED_INT_SAMPLER_2D                  
    toGL (GLSamplerUniform (IntSampler Unsigned) Sampler3D          ) = GL_UNSIGNED_INT_SAMPLER_3D                  
    toGL (GLSamplerUniform (IntSampler Unsigned) Sampler2DRect      ) = GL_UNSIGNED_INT_SAMPLER_2D_RECT             
    toGL (GLSamplerUniform (IntSampler Unsigned) SamplerCube        ) = GL_UNSIGNED_INT_SAMPLER_CUBE                
    toGL (GLSamplerUniform (IntSampler Unsigned) SamplerBuffer      ) = GL_UNSIGNED_INT_SAMPLER_BUFFER              
    toGL (GLSamplerUniform (IntSampler Unsigned) Sampler1DArray     ) = GL_UNSIGNED_INT_SAMPLER_1D_ARRAY            
    toGL (GLSamplerUniform (IntSampler Unsigned) Sampler2DArray     ) = GL_UNSIGNED_INT_SAMPLER_2D_ARRAY            
    toGL (GLSamplerUniform (IntSampler Unsigned) Multisampler2D     ) = GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE      
    toGL (GLSamplerUniform (IntSampler Unsigned) Multisampler2DArray) = GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
    toGL (GLImageUniform FloatSampler Sampler1D          ) = GL_IMAGE_1D                   
    toGL (GLImageUniform FloatSampler Sampler2D          ) = GL_IMAGE_2D                   
    toGL (GLImageUniform FloatSampler Sampler3D          ) = GL_IMAGE_3D                   
    toGL (GLImageUniform FloatSampler Sampler2DRect      ) = GL_IMAGE_2D_RECT              
    toGL (GLImageUniform FloatSampler SamplerCube        ) = GL_IMAGE_CUBE                 
    toGL (GLImageUniform FloatSampler SamplerBuffer      ) = GL_IMAGE_BUFFER               
    toGL (GLImageUniform FloatSampler Sampler1DArray     ) = GL_IMAGE_1D_ARRAY             
    toGL (GLImageUniform FloatSampler Sampler2DArray     ) = GL_IMAGE_2D_ARRAY             
    toGL (GLImageUniform FloatSampler Multisampler2D     ) = GL_IMAGE_2D_MULTISAMPLE       
    toGL (GLImageUniform FloatSampler Multisampler2DArray) = GL_IMAGE_2D_MULTISAMPLE_ARRAY 
    toGL (GLImageUniform (IntSampler Signed) Sampler1D          ) = GL_INT_IMAGE_1D                  
    toGL (GLImageUniform (IntSampler Signed) Sampler2D          ) = GL_INT_IMAGE_2D                  
    toGL (GLImageUniform (IntSampler Signed) Sampler3D          ) = GL_INT_IMAGE_3D                  
    toGL (GLImageUniform (IntSampler Signed) Sampler2DRect      ) = GL_INT_IMAGE_2D_RECT             
    toGL (GLImageUniform (IntSampler Signed) SamplerCube        ) = GL_INT_IMAGE_CUBE                
    toGL (GLImageUniform (IntSampler Signed) SamplerBuffer      ) = GL_INT_IMAGE_BUFFER              
    toGL (GLImageUniform (IntSampler Signed) Sampler1DArray     ) = GL_INT_IMAGE_1D_ARRAY            
    toGL (GLImageUniform (IntSampler Signed) Sampler2DArray     ) = GL_INT_IMAGE_2D_ARRAY            
    toGL (GLImageUniform (IntSampler Signed) Multisampler2D     ) = GL_INT_IMAGE_2D_MULTISAMPLE      
    toGL (GLImageUniform (IntSampler Signed) Multisampler2DArray) = GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
    toGL (GLImageUniform (IntSampler Unsigned) Sampler1D          ) = GL_UNSIGNED_INT_IMAGE_1D                   
    toGL (GLImageUniform (IntSampler Unsigned) Sampler2D          ) = GL_UNSIGNED_INT_IMAGE_2D                   
    toGL (GLImageUniform (IntSampler Unsigned) Sampler3D          ) = GL_UNSIGNED_INT_IMAGE_3D                   
    toGL (GLImageUniform (IntSampler Unsigned) Sampler2DRect      ) = GL_UNSIGNED_INT_IMAGE_2D_RECT              
    toGL (GLImageUniform (IntSampler Unsigned) SamplerCube        ) = GL_UNSIGNED_INT_IMAGE_CUBE                 
    toGL (GLImageUniform (IntSampler Unsigned) SamplerBuffer      ) = GL_UNSIGNED_INT_IMAGE_BUFFER               
    toGL (GLImageUniform (IntSampler Unsigned) Sampler1DArray     ) = GL_UNSIGNED_INT_IMAGE_1D_ARRAY             
    toGL (GLImageUniform (IntSampler Unsigned) Sampler2DArray     ) = GL_UNSIGNED_INT_IMAGE_2D_ARRAY             
    toGL (GLImageUniform (IntSampler Unsigned) Multisampler2D     ) = GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE       
    toGL (GLImageUniform (IntSampler Unsigned) Multisampler2DArray) = GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY 
    toGL GLAtomicCounter = GL_UNSIGNED_INT_ATOMIC_COUNTER


instance FromGL GLUniformType where
    fromGL x | Just (GLScalarAttrib s)     <- fromGL x = Just $ GLScalarUniform s
             | Just (GLVectorAttrib s n)   <- fromGL x = Just $ GLVectorUniform s n
             | Just (GLMatrixAttrib s n m) <- fromGL x = Just $ GLMatrixUniform s n m
    fromGL GL_BOOL      = Just $ GLBoolUniform Nothing
    fromGL GL_BOOL_VEC2 = Just $ GLBoolUniform (Just Two)
    fromGL GL_BOOL_VEC3 = Just $ GLBoolUniform (Just Three)
    fromGL GL_BOOL_VEC4 = Just $ GLBoolUniform (Just Four)
    fromGL GL_SAMPLER_1D                   = Just $ GLSamplerUniform FloatSampler Sampler1D                
    fromGL GL_SAMPLER_2D                   = Just $ GLSamplerUniform FloatSampler Sampler2D
    fromGL GL_SAMPLER_3D                   = Just $ GLSamplerUniform FloatSampler Sampler3D
    fromGL GL_SAMPLER_2D_RECT              = Just $ GLSamplerUniform FloatSampler Sampler2DRect
    fromGL GL_SAMPLER_CUBE                 = Just $ GLSamplerUniform FloatSampler SamplerCube
    fromGL GL_SAMPLER_BUFFER               = Just $ GLSamplerUniform FloatSampler SamplerBuffer
    fromGL GL_SAMPLER_1D_ARRAY             = Just $ GLSamplerUniform FloatSampler Sampler1DArray
    fromGL GL_SAMPLER_2D_ARRAY             = Just $ GLSamplerUniform FloatSampler Sampler2DArray
    fromGL GL_SAMPLER_2D_MULTISAMPLE       = Just $ GLSamplerUniform FloatSampler Multisampler2D
    fromGL GL_SAMPLER_2D_MULTISAMPLE_ARRAY = Just $ GLSamplerUniform FloatSampler Multisampler2DArray
    fromGL GL_SAMPLER_1D_SHADOW       = Just $ GLShadowSampler ShadowSampler1D
    fromGL GL_SAMPLER_2D_SHADOW       = Just $ GLShadowSampler ShadowSampler2D
    fromGL GL_SAMPLER_2D_RECT_SHADOW  = Just $ GLShadowSampler ShadowSampler2DRect
    fromGL GL_SAMPLER_CUBE_SHADOW     = Just $ GLShadowSampler ShadowSamplerCube
    fromGL GL_SAMPLER_1D_ARRAY_SHADOW = Just $ GLShadowSampler ShadowSampler1DArray
    fromGL GL_SAMPLER_2D_ARRAY_SHADOW = Just $ GLShadowSampler ShadowSampler2DArray
    fromGL GL_INT_SAMPLER_1D                   = Just $ GLSamplerUniform (IntSampler Signed) Sampler1D
    fromGL GL_INT_SAMPLER_2D                   = Just $ GLSamplerUniform (IntSampler Signed) Sampler2D
    fromGL GL_INT_SAMPLER_3D                   = Just $ GLSamplerUniform (IntSampler Signed) Sampler3D
    fromGL GL_INT_SAMPLER_2D_RECT              = Just $ GLSamplerUniform (IntSampler Signed) Sampler2DRect
    fromGL GL_INT_SAMPLER_CUBE                 = Just $ GLSamplerUniform (IntSampler Signed) SamplerCube
    fromGL GL_INT_SAMPLER_BUFFER               = Just $ GLSamplerUniform (IntSampler Signed) SamplerBuffer
    fromGL GL_INT_SAMPLER_1D_ARRAY             = Just $ GLSamplerUniform (IntSampler Signed) Sampler1DArray
    fromGL GL_INT_SAMPLER_2D_ARRAY             = Just $ GLSamplerUniform (IntSampler Signed) Sampler2DArray
    fromGL GL_INT_SAMPLER_2D_MULTISAMPLE       = Just $ GLSamplerUniform (IntSampler Signed) Multisampler2D
    fromGL GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = Just $ GLSamplerUniform (IntSampler Signed) Multisampler2DArray
    fromGL GL_UNSIGNED_INT_SAMPLER_1D                   = Just $ GLSamplerUniform (IntSampler Unsigned) Sampler1D
    fromGL GL_UNSIGNED_INT_SAMPLER_2D                   = Just $ GLSamplerUniform (IntSampler Unsigned) Sampler2D
    fromGL GL_UNSIGNED_INT_SAMPLER_3D                   = Just $ GLSamplerUniform (IntSampler Unsigned) Sampler3D
    fromGL GL_UNSIGNED_INT_SAMPLER_2D_RECT              = Just $ GLSamplerUniform (IntSampler Unsigned) Sampler2DRect
    fromGL GL_UNSIGNED_INT_SAMPLER_CUBE                 = Just $ GLSamplerUniform (IntSampler Unsigned) SamplerCube
    fromGL GL_UNSIGNED_INT_SAMPLER_BUFFER               = Just $ GLSamplerUniform (IntSampler Unsigned) SamplerBuffer
    fromGL GL_UNSIGNED_INT_SAMPLER_1D_ARRAY             = Just $ GLSamplerUniform (IntSampler Unsigned) Sampler1DArray
    fromGL GL_UNSIGNED_INT_SAMPLER_2D_ARRAY             = Just $ GLSamplerUniform (IntSampler Unsigned) Sampler2DArray
    fromGL GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE       = Just $ GLSamplerUniform (IntSampler Unsigned) Multisampler2D
    fromGL GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = Just $ GLSamplerUniform (IntSampler Unsigned) Multisampler2DArray
    fromGL GL_IMAGE_1D                   = Just $ GLImageUniform FloatSampler Sampler1D
    fromGL GL_IMAGE_2D                   = Just $ GLImageUniform FloatSampler Sampler2D
    fromGL GL_IMAGE_3D                   = Just $ GLImageUniform FloatSampler Sampler3D
    fromGL GL_IMAGE_2D_RECT              = Just $ GLImageUniform FloatSampler Sampler2DRect
    fromGL GL_IMAGE_CUBE                 = Just $ GLImageUniform FloatSampler SamplerCube
    fromGL GL_IMAGE_BUFFER               = Just $ GLImageUniform FloatSampler SamplerBuffer
    fromGL GL_IMAGE_1D_ARRAY             = Just $ GLImageUniform FloatSampler Sampler1DArray
    fromGL GL_IMAGE_2D_ARRAY             = Just $ GLImageUniform FloatSampler Sampler2DArray
    fromGL GL_IMAGE_2D_MULTISAMPLE       = Just $ GLImageUniform FloatSampler Multisampler2D
    fromGL GL_IMAGE_2D_MULTISAMPLE_ARRAY = Just $ GLImageUniform FloatSampler Multisampler2DArray
    fromGL GL_INT_IMAGE_1D                   = Just $ GLImageUniform (IntSampler Signed) Sampler1D
    fromGL GL_INT_IMAGE_2D                   = Just $ GLImageUniform (IntSampler Signed) Sampler2D
    fromGL GL_INT_IMAGE_3D                   = Just $ GLImageUniform (IntSampler Signed) Sampler3D
    fromGL GL_INT_IMAGE_2D_RECT              = Just $ GLImageUniform (IntSampler Signed) Sampler2DRect
    fromGL GL_INT_IMAGE_CUBE                 = Just $ GLImageUniform (IntSampler Signed) SamplerCube
    fromGL GL_INT_IMAGE_BUFFER               = Just $ GLImageUniform (IntSampler Signed) SamplerBuffer
    fromGL GL_INT_IMAGE_1D_ARRAY             = Just $ GLImageUniform (IntSampler Signed) Sampler1DArray
    fromGL GL_INT_IMAGE_2D_ARRAY             = Just $ GLImageUniform (IntSampler Signed) Sampler2DArray
    fromGL GL_INT_IMAGE_2D_MULTISAMPLE       = Just $ GLImageUniform (IntSampler Signed) Multisampler2D
    fromGL GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY = Just $ GLImageUniform (IntSampler Signed) Multisampler2DArray
    fromGL GL_UNSIGNED_INT_IMAGE_1D                   = Just $ GLImageUniform (IntSampler Unsigned) Sampler1D
    fromGL GL_UNSIGNED_INT_IMAGE_2D                   = Just $ GLImageUniform (IntSampler Unsigned) Sampler2D
    fromGL GL_UNSIGNED_INT_IMAGE_3D                   = Just $ GLImageUniform (IntSampler Unsigned) Sampler3D
    fromGL GL_UNSIGNED_INT_IMAGE_2D_RECT              = Just $ GLImageUniform (IntSampler Unsigned) Sampler2DRect
    fromGL GL_UNSIGNED_INT_IMAGE_CUBE                 = Just $ GLImageUniform (IntSampler Unsigned) SamplerCube
    fromGL GL_UNSIGNED_INT_IMAGE_BUFFER               = Just $ GLImageUniform (IntSampler Unsigned) SamplerBuffer
    fromGL GL_UNSIGNED_INT_IMAGE_1D_ARRAY             = Just $ GLImageUniform (IntSampler Unsigned) Sampler1DArray
    fromGL GL_UNSIGNED_INT_IMAGE_2D_ARRAY             = Just $ GLImageUniform (IntSampler Unsigned) Sampler2DArray
    fromGL GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE       = Just $ GLImageUniform (IntSampler Unsigned) Multisampler2D
    fromGL GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY = Just $ GLImageUniform (IntSampler Unsigned) Multisampler2DArray
    fromGL GL_UNSIGNED_INT_ATOMIC_COUNTER             = Just GLAtomicCounter
    fromGL _ = Nothing


