module Graphics.GL.Low.ImageFormat where

import Graphics.GL

import Graphics.GL.Low.Classes

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

instance InternalFormat RGB where
  internalFormat _ = GL_RGB
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
