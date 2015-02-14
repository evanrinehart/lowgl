module Graphics.GL.Low.Common where

import Graphics.GL

import Graphics.GL.Low.Cube

cubeSideCodes :: Cube GLenum
cubeSideCodes = Cube
  { cubeLeft   = GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  , cubeRight  = GL_TEXTURE_CUBE_MAP_POSITIVE_X
  , cubeTop    = GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  , cubeBottom = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  , cubeFront  = GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  , cubeBack   = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z }

