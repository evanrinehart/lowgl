module Graphics.GL.Low.Stencil (

-- | The stencil test is like a configurable depth test with a dedicated
-- additional buffer. Like the depth test, if the stencil test fails then the
-- pixel being tested will not be rendered. The stencil test happens before the
-- depth test, if it's enabled. For a given pixel, the stencil test passes if
-- the following computation
--
-- > (ref & mask) `stencilOp` (buf & mask)
--
-- computes true. The variables ref and mask are configurable constants, buf is
-- the value in the stencil buffer at the given pixel, and stencilOp is a
-- configurable comparison operation. The stencil op can also be set to pass
-- 'Always' or 'Never'.
--
-- The stencil buffer may be modified in various ways on the following events:
--
-- - When the stencil test fails or
-- - When the stencil test passes then the depth test fails or
-- - When both tests pass.

  enableStencil,
  disableStencil,
  clearStencilBuffer,
  basicStencil,
  Stencil(..),
  StencilFunc(..),
  StencilOp(..)
) where

import Data.Bits
import Data.Word

import Graphics.GL
import Graphics.GL.Low.Classes

-- | Enable the stencil test with a set of operating parameters.
enableStencil :: Stencil -> IO ()
enableStencil (Stencil f r m op1 op2 op3) = do
  glStencilFunc (toGL f) (fromIntegral r) (fromIntegral m)
  glStencilOp (toGL op1) (toGL op2) (toGL op3)
  glEnable GL_STENCIL_TEST

-- | Disable the stencil test and updates to the stencil buffer, if one exists.
disableStencil :: IO ()
disableStencil = glDisable GL_STENCIL_TEST

-- | Clear the stencil buffer with all zeros.
clearStencilBuffer :: IO ()
clearStencilBuffer = glClear GL_STENCIL_BUFFER_BIT

-- | In this basic configuration of the stencil, anything rendered will
-- create a silhouette of 1s in the stencil buffer. Attempting to render a
-- second time into the silhouette will have no effect because the stencil
-- test will fail (ref=1 isn't greater than buffer=1).
--
-- @
-- def { func = Greater
--     , ref = 1
--     , onBothPass = Replace }
-- @
basicStencil :: Stencil
basicStencil = defaultStencil
  { func = Greater
  , ref = 1
  , onBothPass = Replace }

-- | Configuration of the stencil test and associated stencil buffer updating.
data Stencil = Stencil
  { func          :: StencilFunc
  , ref           :: Int
  , mask          :: Word
  , onStencilFail :: StencilOp
  , onDepthFail   :: StencilOp
  , onBothPass    :: StencilOp }

-- | The default state of the stencil, if it were simply enabled, would be
-- to always pass and update nothing in the buffer. It would have no effect
-- on rendering.
defaultStencil :: Stencil
defaultStencil = Stencil
  { func = Always
  , ref  = 0
  , mask = complement 0
  , onStencilFail = Keep
  , onDepthFail   = Keep
  , onBothPass    = Keep }

-- the proper implementation of toList f is the one that for any
-- g :: t a -> [a] you can make a unique k such that g = k . f
-- jle`

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
