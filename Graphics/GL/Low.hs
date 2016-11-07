-- | Basic low-level GL wrapper and reference.
module Graphics.GL.Low (

  -- * Overview
  -- | This library exposes a simplified subset of OpenGL that I hope is
  -- complete enough for following tutorials and making simple games or demos.
  --
  -- For a whirlwind tour of the machinery behind GL see the module:
  -- "Graphics.GL.Low.EntirePictureUpFront"
  --
  -- This library uses the `gl' package for raw bindings to OpenGL and the
  -- `linear' package for matrices.
  --
  -- See submodules for specialized documentation of each subsystem.
  --
  -- @"Graphics.GL.Low.VAO"@
  --
  -- @"Graphics.GL.Low.BufferObject"@
  --
  -- @"Graphics.GL.Low.Shader"@
  --
  -- @"Graphics.GL.Low.VertexAttrib"@
  --
  -- @"Graphics.GL.Low.Texture"@
  --
  -- @"Graphics.GL.Low.Render"@
  --
  -- @"Graphics.GL.Low.Color"@
  --
  -- @"Graphics.GL.Low.Depth"@
  -- 
  -- @"Graphics.GL.Low.Stencil"@
  --
  -- @"Graphics.GL.Low.Blending"@
  --
  -- @"Graphics.GL.Low.Framebuffer"@

  -- * VAO
  -- | See also "Graphics.GL.Low.VAO".
  newVAO,
  bindVAO,
  deleteVAO,
  VAO,

  -- * Buffer Objects
  -- | See also "Graphics.GL.Low.BufferObject".
  newBufferObject,
  bindVBO,
  bindElementArray,
  updateVBO,
  updateElementArray,
  deleteBufferObject,
  BufferObject,
  UsageHint(..),

  -- * Shader Program
  -- | See also "Graphics.GL.Low.Shader".
  newProgram,
  newProgramSafe,
  useProgram,
  deleteProgram,
  setUniform1f, 
  setUniform2f,
  setUniform3f,
  setUniform4f,
  setUniform1i,
  setUniform2i,
  setUniform3i,
  setUniform4i,
  setUniform22,
  setUniform33,
  setUniform44,
  Program,
  ProgramError(..),

  -- ** Vertex Attributes
  -- | See also "Graphics.GL.Low.VertexAttrib".
  setVertexLayout,
  VertexLayout(..),
  DataType(..),


  -- * Textures
  -- | See also "Graphics.GL.Low.Texture".
  Texture,
  newTexture2D,
  newCubeMap,
  newEmptyTexture2D,
  newEmptyCubeMap,
  deleteTexture,
  setActiveTextureUnit,
  bindTexture2D,
  bindTextureCubeMap,
  setTex2DFiltering,
  setCubeMapFiltering,
  setTex2DWrapping,
  setCubeMapWrapping,
  Cube(..),
  Filtering(..),
  Wrapping(..),

  -- * Rendering
  --
  -- ** Primitives
  -- | See also "Graphics.GL.Low.Render".
  drawPoints,
  drawLines,
  drawLineStrip,
  drawLineLoop,
  drawTriangles,
  drawTriangleStrip,
  drawTriangleFan,
  drawIndexedPoints,
  drawIndexedLines,
  drawIndexedLineStrip,
  drawIndexedLineLoop,
  drawIndexedTriangles,
  drawIndexedTriangleStrip,
  drawIndexedTriangleFan,
  setViewport,
  enableScissorTest,
  disableScissorTest,
  enableCulling,
  disableCulling,
  Viewport(..),
  Culling(..),
  IndexFormat(..),

  -- ** Color Buffer
  -- | See also "Graphics.GL.Low.Color".
  enableColorWriting,
  disableColorWriting,
  clearColorBuffer,

  -- ** Depth Test
  -- | See also "Graphics.GL.Low.Depth".
  enableDepthTest,
  disableDepthTest,
  clearDepthBuffer,

  -- ** Stencil Test
  -- | See also "Graphics.GL.Low.Stencil".
  enableStencil,
  disableStencil,
  clearStencilBuffer,
  basicStencil,
  Stencil(..),
  StencilFunc(..),
  StencilOp(..),

  -- ** Blending
  -- | See also "Graphics.GL.Low.Blending".

  enableBlending,
  disableBlending,
  basicBlending,
  Blending(..),
  BlendFactor(..),
  BlendEquation(..),

  -- * Framebuffers
  -- | See also "Graphics.GL.Low.Framebuffer".
  FBO,
  newFBO,
  bindFBO,
  bindDefaultFramebuffer,
  deleteFBO,
  attachTex2D,
  attachCubeMap,
  attachRBO,

  -- * Renderbuffers
  RBO,
  newRBO,
  deleteRBO,

  -- * Errors
  GLError(..),
  getGLError,
  assertNoGLError,

  -- * Image Formats
  ImageFormat(..),

) where

import Graphics.GL.Low.Classes
import Graphics.GL.Low.VAO
import Graphics.GL.Low.BufferObject
import Graphics.GL.Low.Shader
import Graphics.GL.Low.VertexAttrib
import Graphics.GL.Low.Texture
import Graphics.GL.Low.Framebuffer
import Graphics.GL.Low.Blending
import Graphics.GL.Low.Color
import Graphics.GL.Low.Depth
import Graphics.GL.Low.Stencil
import Graphics.GL.Low.Render
import Graphics.GL.Low.Cube
import Graphics.GL.Low.Error
