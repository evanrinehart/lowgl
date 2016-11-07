module Graphics.GL.Low.Common where

import Graphics.GL

import Graphics.GL.Low.Types
import Graphics.GL.Low.Cube

attachmentPointForImageFormat :: ImageFormat -> GLenum
attachmentPointForImageFormat format = case format of
  RGB             -> GL_COLOR_ATTACHMENT0
  RGBA            -> GL_COLOR_ATTACHMENT0
  Alpha           -> GL_COLOR_ATTACHMENT0
  Luminance       -> GL_COLOR_ATTACHMENT0
  LuminanceAlpha  -> GL_COLOR_ATTACHMENT0
  Depth24         -> GL_DEPTH_ATTACHMENT
  Depth24Stencil8 -> GL_DEPTH_STENCIL_ATTACHMENT

cubeSideCodes :: Cube GLenum
cubeSideCodes = Cube
  { cubeLeft   = GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  , cubeRight  = GL_TEXTURE_CUBE_MAP_POSITIVE_X
  , cubeTop    = GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  , cubeBottom = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  , cubeFront  = GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  , cubeBack   = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z }
