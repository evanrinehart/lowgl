-- | When blending is enabled, colors written to the color buffer will be
-- blended using a formula with the color already there. The three options
-- for the formula are:
--
-- - S*s + D*d ('FuncAdd', the default)
-- - S*s - D*d ('FuncSubtract')
-- - D*d - S*s ('FuncReverseSubtract')
--
-- where S and D are source and destination color components respectively. The
-- factors s and d are computed blending factors which can depend on the alpha
-- component of the source pixel, the destination pixel, or a specified
-- constant color. See 'basicBlending' for a common choice.
--
-- The order of rendering matters when using blending. The farther-away
-- primitives should be rendered first to get transparent materials to look
-- right. This means a depth test is unhelpful when using blending. Also
-- blending many layers of transparent primitives can significantly degrade
-- performance. For these reasons transparency effects may be better
-- accomplished with an off-screen rendering pass followed by a suitable
-- shader.
--
-- @
-- blending example program here
-- @

module Graphics.GL.Low.Blending where

import Data.Default
import Graphics.GL

import Graphics.GL.Low.Classes

-- | Enable blending with the specified blending parameters.
enableBlending :: Blending -> IO ()
enableBlending (Blending s d f (r,g,b,a)) = do
  glBlendFunc (toGL s) (toGL d)
  glBlendEquation (toGL f)
  let c = realToFrac
  glBlendColor (c r) (c g) (c b) (c a)
  glEnable GL_BLEND

-- | Disable alpha blending.
disableBlending :: IO ()
disableBlending = glDisable GL_BLEND

-- | This blending configuration is suitable for ordinary alpha blending
-- transparency effects.
--
-- @
-- Blending
--   { sFactor   = BlendSourceAlpha
--   , dFactor   = BlendOneMinusSourceAlpha
--   , blendFunc = FuncAdd }
-- @
basicBlending :: Blending
basicBlending = def
  { sFactor = BlendSourceAlpha
  , dFactor = BlendOneMinusSourceAlpha }


-- | Blending parameters.
data Blending = Blending
  { sFactor :: BlendFactor
  , dFactor :: BlendFactor
  , blendFunc :: BlendEquation
  , blendColor :: (Float,Float,Float,Float) }

-- | The default blending parameters have no effect if enabled. The result
-- will be no blending effect.
instance Default Blending where
  def = Blending
    { sFactor = BlendOne
    , dFactor = BlendZero
    , blendFunc = FuncAdd
    , blendColor = (0,0,0,0) }

-- | Blending functions.
data BlendEquation =
  FuncAdd | -- ^ the default
  FuncSubtract |
  FuncReverseSubtract
    deriving Show

instance Default BlendEquation where
  def = FuncAdd

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


