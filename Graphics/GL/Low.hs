-- | Basic low-level GL wrapper and reference.
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
  -- "Graphics.GL.Low.EntirePictureUpFront"
  --
  -- This library uses the `gl' package for raw bindings to OpenGL and the
  -- `linear' package for matrices.
  --
  --
  -- See specific modules for topic-specific docs and example code:
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
  loadVBO,
  updateVBO,
  bindElementArray,
  loadElementArray,
  updateElementArray,
  deleteBufferObject,
  VBO,
  ElementArray,
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
  GLAttribType(..),
  GLFloatType(..),
  Signedness(..),
  GLScalarType(..),
  GLVectorSize(..),


  -- * Textures
  -- | See also "Graphics.GL.Low.Texture".
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
  Tex2D,
  CubeMap,
  Dimensions(..),
  Cube(..),
  Side,
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
  DefaultFramebuffer(..),
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
  assertNoGLError,

  -- * Image Formats
  Alpha,
  Luminance,
  LuminanceAlpha,
  RGB,
  RGBA,
  Depth24,
  Depth24Stencil8,

  -- * Classes
  ToGL(..),
  FromGL(..),
  InternalFormat(..),
  Framebuffer(..),
  Texture(..),
  Attachable(..)

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
import Graphics.GL.Low.ImageFormat
import Graphics.GL.Low.Cube
import Graphics.GL.Low.Error
import Graphics.GL.Low.Internal.Types
